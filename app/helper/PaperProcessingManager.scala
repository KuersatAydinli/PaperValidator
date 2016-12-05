package helper

import java.io.{File, FileWriter}

import ch.uzh.ifi.pdeboer.pplib.hcomp.HTMLQuery
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.dao.BallotDAO
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.integrationtest.console.ConsoleIntegrationTest
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.persistence.{DBSettings, Permutation}
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.report.Report
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.{Algorithm250, BallotPortalAdapter}
import ch.uzh.ifi.pdeboer.pplib.util.CollectionUtils._
import controllers.routes
import helper.email.MailTemplates
import helper.pdfpreprocessing.PreprocessPDF
import helper.questiongenerator.HCompNew
import helper.statcheck.Statchecker
import models._
import org.codehaus.plexus.util.FileUtils
import org.joda.time.DateTime
import play.api.db.Database
import play.api.libs.concurrent.Execution.Implicits._
import play.api.{Configuration, Logger}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source


/**
  * Created by manuel on 21.04.2016.
  */
object PaperProcessingManager {
  var BYPASS_CROWD_PROCESSING = false
  var AUTO_ANNOTATION = true

  var isRunning = false

  def run(database: Database, configuration: Configuration, papersService: PapersService,
          questionService: QuestionService, method2AssumptionService: Method2AssumptionService,
          paperResultService: PaperResultService, paperMethodService: PaperMethodService,
          permutationsService: PermutationsService, answerService: AnswerService,
          conferenceSettingsService: ConferenceSettingsService): Boolean = {
    if (!synchronized(isRunning)) {
      synchronized {
        isRunning = true
      }
      val processPapers: Future[Int] = Future {
        val papersToProcess = papersService.findProcessablePapers()
        papersToProcess.foreach(paper =>
          try {
            processPaper(database, configuration, papersService, questionService, method2AssumptionService,
              paperResultService, paperMethodService, permutationsService, answerService, conferenceSettingsService,
              paper)
          } catch {
            case error: Throwable => {
              Logger.info("ERROR!!!!!!")
              Logger.info(error.getMessage + " " + error.getStackTrace.mkString("\n"))
              val errorMsg = error.getStackTrace.mkString("\n") + "\n"
              PaperProcessingManager.writePaperLog(errorMsg, paper.secret)
              papersService.updateStatus(paper.id.get, Papers.STATUS_ERROR)
            }
          }
        )
        papersService.findProcessablePapers().length
      }
      processPapers onComplete {
        case newPapers => {
          synchronized {
            isRunning = false
          }
          if (newPapers.get > 0) {
            run(database, configuration, papersService, questionService, method2AssumptionService,
              paperResultService, paperMethodService, permutationsService, answerService, conferenceSettingsService)
          }
        }
      }
    }
    true
  }

  def processPaper(database: Database, configuration: Configuration, papersService: PapersService,
                   questionService: QuestionService, method2AssumptionService: Method2AssumptionService,
                   paperResultService: PaperResultService, paperMethodService: PaperMethodService,
                   permutationsServcie: PermutationsService, answerService: AnswerService,
                   conferenceSettingsService: ConferenceSettingsService, paper: Papers) = {
    Logger.debug("processPaper: " + paper.name + " " + DateTime.now())
    if (paper.status == Papers.STATUS_NEW) {
      processNewPaper(database, configuration, papersService, questionService, paperResultService, paperMethodService, permutationsServcie, answerService, conferenceSettingsService, paper)
    } else if (paper.status == Papers.STATUS_IN_PPLIB_QUEUE) {
      confirmExistingPaper(configuration, papersService, questionService, method2AssumptionService, paperResultService, paperMethodService, answerService, conferenceSettingsService, paper)
    }
  }

  def confirmExistingPaper(configuration: Configuration, papersService: PapersService, questionService: QuestionService, method2AssumptionService: Method2AssumptionService, paperResultService: PaperResultService, paperMethodService: PaperMethodService, answerService: AnswerService, conferenceSettingsService: ConferenceSettingsService, paper: Papers): Unit = {
    writePaperLog("Run Question Generator\n", paper.secret)
    startRunning(questionService, method2AssumptionService, paper)
    papersService.updateStatus(paper.id.get, Papers.STATUS_COMPLETED)
    if (AUTO_ANNOTATION) PaperAnnotator.annotatePaper(configuration, answerService, papersService, conferenceSettingsService,
      paperResultService, paperMethodService, paper, false)
    writePaperLog("Finish and Notify Crowdwork\n", paper.secret)
    val paperLink = configuration.getString("hcomp.ballot.baseURL").get + routes.Paper.show(paper.id.get, paper.secret).url
    MailTemplates.sendPaperCompletedMail(paper.name, paperLink, paper.email)
    writePaperLog("Clean Up\n", paper.secret)
    //cleanUpTmpDir(paper)
    writePaperLog("<b>Completed!</b>\n\n", paper.secret)
  }

  private def processNewPaper(database: Database, configuration: Configuration, papersService: PapersService, questionService: QuestionService, paperResultService: PaperResultService, paperMethodService: PaperMethodService, permutationsServcie: PermutationsService, answerService: AnswerService, conferenceSettingsService: ConferenceSettingsService, paper: Papers): Unit = {
    writePaperLog("<b>Start</b> Analysis\n", paper.secret)
    Commons.generateCoverFile(paper)
    writePaperLog("Run StatChecker\n", paper.secret)
    try {
      val statCheck = Future {
        Statchecker.run(paper, paperResultService)
      }
      Await.result(statCheck, 120 seconds)
    } catch {
      case e: Throwable => {
        Logger.info("StatCheck Timeout/Error!!!")
        writePaperLog("StatChecker Timeout/Error!!!\n", paper.secret)
        writePaperLog(e.getStackTrace.mkString("\n") + "\n", paper.secret)
      }
    }
    writePaperLog("Run Layout Checker\n", paper.secret)
    try {
      val layoutCheck = Future {
        LayoutChecker.check(paper, paperResultService)
      }
      Await.result(layoutCheck, 120 seconds)
    } catch {
      case e: Throwable => {
        Logger.info("Layout Check Timeout/Error!!!")
        writePaperLog("Layout Checker Timeout/Error!!!\n", paper.secret)
        writePaperLog(e.getStackTrace.mkString("\n") + "\n", paper.secret)
      }
    }
    if (AUTO_ANNOTATION) {
      writePaperLog("Getting annotations of paper\n", paper.secret)
      PaperAnnotator.annotatePaper(configuration, answerService, papersService, conferenceSettingsService,
        paperResultService, paperMethodService, paper, glossaryWithIDMode = false)
      println("done annotating")
    }

    writePaperLog("Run PreprocessPDF\n", paper.secret)
    val process = Future {
      PreprocessPDF.start(database, paperMethodService, paper)
    }
    val permutations = Await.result(process, 700 seconds)
    if (permutations > 0) {
      writePaperLog("<b>" + permutations + " Permutation(s)</b> Found\n", paper.secret)
      papersService.updateStatus(paper.id.get, Papers.STATUS_AWAIT_CONFIRMATION)
      papersService.updatePermutations(paper.id.get, permutations)
      /*
      if (BYPASS_CROWD_PROCESSING) {
        skipCrowdWork(paper.id.get, paper.secret, questionService, permutationsServcie, answerService)
        papersService.updateStatus(paper.id.get, Papers.STATUS_COMPLETED)
        if (AUTO_ANNOTATION) PaperAnnotator.annotatePaper(configuration, answerService, papersService, conferenceSettingsService,
          paperResultService, paperMethodService, paper, false)
      }*/

    } else {
      writePaperLog("<b>No Permutations</b> Found\n", paper.secret)
      papersService.updateStatus(paper.id.get, Papers.STATUS_COMPLETED)
      if (AUTO_ANNOTATION) PaperAnnotator.annotatePaper(configuration, answerService, papersService, conferenceSettingsService,
        paperResultService, paperMethodService, paper, false)
    }
    writePaperLog("Finish and Notify Analysis\n", paper.secret)
    val paperLink = configuration.getString("hcomp.ballot.baseURL").get + routes.Paper.confirmPaper(paper.id.get, paper.secret).url
    MailTemplates.sendPaperAnalyzedMail(paper.name, paperLink, permutations, paper.email)
  }

  def cleanUpTmpDir(paper: Papers): Unit = {
    FileUtils.deleteDirectory(new File(PreprocessPDF.OUTPUT_DIR + "/" + Commons.getSecretHash(paper.secret)))
  }

  def startRunning(questionService: QuestionService, method2AssumptionService: Method2AssumptionService, paper: Papers): Unit = {
    val DEFAULT_TEMPLATE_ID: Long = 1L

    DBSettings.initialize()
    val dao = new BallotDAO
    HCompNew.autoloadConfiguredPortals()
    val ballotPortalAdapter = HCompNew(BallotPortalAdapter.PORTAL_KEY)
    val algorithm250 = Algorithm250(dao, ballotPortalAdapter, method2AssumptionService)

    if (questionService.findById(DEFAULT_TEMPLATE_ID).isEmpty) {
      createFirstTimePaperTemplate(questionService, method2AssumptionService, paper, DEFAULT_TEMPLATE_ID, algorithm250)
    }

    if (!dao.getAllPermutations().exists(p => p.paperId.toInt == paper.id.get)) {
      //see if we have already loaded this paper before
      Logger.info("Loading new permutations")
      dao.loadPermutationsCSV(PreprocessPDF.OUTPUT_DIR + "/" + Commons.getSecretHash(paper.secret) + "/permutations.csv",
        paper.id.get)
    } else {
      Logger.info(s"found permutations for paper ${paper.name}, skipping import")
    }
    Logger.info("Removing state information of previous runs")
    new File("state").listFiles().foreach(f => f.delete())

    val groups = dao.getAllPermutations().filter(_.id != ConsoleIntegrationTest.DEFAULT_TEMPLATE_ID).groupBy(gr => {
      gr.groupName.split("/").apply(0)
    }).map(g => (g._1, g._2.sortBy(_.distanceMinIndexMax))).toList

    groups.mpar.foreach(group => {
      group._2.foreach(permutation => {
        if (dao.getPermutationById(permutation.id).map(_.state).getOrElse(-1) == 0) {
          Logger.debug(s"starting 250 for ${paper.name} -> permutation ${permutation.id}")
          algorithm250.executePermutation(paper.conferenceId, permutation)
        }
      })
    })

    Report.writeCSVReport(dao)
    Report.writeCSVReportAllAnswers(dao)
  }

  def createFirstTimePaperTemplate(questionService: QuestionService, method2AssumptionService: Method2AssumptionService, paper: Papers, DEFAULT_TEMPLATE_ID: Long, algorithm250: Algorithm250): Unit = {
    Logger.info("templateInit")
    val template: File = new File("public/template/perm.csv")
    val dao = new BallotDAO
    if (template.exists()) {
      val templatePermutations = Source.fromFile(template).getLines().drop(1).map(l => {
        val perm: Permutation = Permutation.fromCSVLine(l)
        dao.createPermutation(perm, paper.id.get)
      })
      Thread.sleep(1000)
      val ballotPortalAdapter = HCompNew(BallotPortalAdapter.PORTAL_KEY)

      templatePermutations.foreach(permutationId => {
        val q = algorithm250.buildQuestion(paper.conferenceId, dao.getPermutationById(permutationId).get, isTemplate = true)
        Logger.info("WriteTemplate")
        ballotPortalAdapter.sendQuery(HTMLQuery(q._2, 1, "Statistical Methods and Prerequisites", ""), q._1)
        Thread.sleep(1000)
      })
      Thread.sleep(1000)
      assert(!templatePermutations.contains(DEFAULT_TEMPLATE_ID), "Our template didn't get ID 1. Please adapt DB. Current template IDs: " + templatePermutations.mkString(","))
    }
    Logger.info("templateInit Done")
    //questionGenerator(questionService, method2AssumptionService, paper)
  }

  def writePaperLog(logMsg: String, secret: String) = {
    val fw = new FileWriter(PreprocessPDF.INPUT_DIR + "/" + Commons.getSecretHash(secret) + "/log.txt", true)
    try {
      fw.write(logMsg)
    }
    finally fw.close()
  }

  def skipCrowdWork(paperId: Int, secret: String, questionService: QuestionService,
                    permutationsServcie: PermutationsService, answerService: AnswerService): Unit = {
    writePaperLog("Skipped Crowd Processing\n", secret)
    val src = Source.fromFile(PreprocessPDF.OUTPUT_DIR + "/" + Commons.getSecretHash(secret) + "/permutations.csv")
    val perms = src.getLines().drop(1).map(_.split(",")).toList
    perms.foreach(p => {
      val permutationId = permutationsServcie.create(p(0), p(1), paperId)
      val questionId = questionService.create("", 1, "", permutationId, "")
      answerService.create(questionId, 1, new DateTime, true, true, true, 10, "", 123)
    })
    writePaperLog("Completet!\n", secret)

  }

}