package controllers

import java.io.{File, PrintWriter}
import javax.inject.Inject

import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.dao.BallotDAO
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.persistence.DBSettings
import com.github.tototoshi.csv.CSVWriter
import com.typesafe.config.ConfigFactory
import helper.Commons
import helper.email.MailTemplates
import helper.pdfpreprocessing.PreprocessPDF
import models._
import play.Configuration
import play.api.Logger
import play.api.mvc.{Action, Controller}

import scala.io.Source

/**
  * Created by manuel on 11.04.2016.
  */
class Conference @Inject()(configuration: Configuration, conferenceService: ConferenceService,
                           conferenceSettingsService: ConferenceSettingsService, methodService: MethodService,
                           assumptionService: AssumptionService, method2AssumptionService: Method2AssumptionService,
                           emailService: EmailService, papersService: PapersService, answerService: AnswerService,
                           paperResultService: PaperResultService, paperMethodService: PaperMethodService
                          ) extends Controller {

  def conferenceCreator = Action {
    val templateDir = new File("statterms/templates")
    Ok(views.html.conference.conferenceCreator(templateDir.list().toList))
  }

  def conferenceCreated = Action(parse.urlFormEncoded) { request =>
    val name = request.body.get("name").get.head
    val email = request.body.get("email").get.head
    val password = request.body.get("password").get.head
    Logger.info("PW Conference" + password)
    val conf = ConfigFactory.load()
    val passwordConfig = conf.getString("admin.access.password")
    Logger.info("PW Config: " + passwordConfig)
    val template = request.body.get("template").get.head
    val secret = Commons.generateSecret()
    if (password == passwordConfig) {
      val id = conferenceService.create(name, email, secret)
      readTemplate(id, template)
      MailTemplates.sendAccountMail(email, configuration, emailService)
      val conferenceLink = configuration.getString("hcomp.ballot.baseURL") + routes.Conference.conferenceEditor(id, secret).url
      MailTemplates.sendConferenceMail(name, conferenceLink, email)
      Ok(views.html.conference.conferenceCreated(name))
    } else {
      Ok("Error: Incorrect Password")
    }
  }

  def readTemplate(conferenceId: Int, templateName: String) = {
    Source.fromFile("statterms/templates/" + templateName + "/methods.csv", "UTF-8").getLines().foreach(line => {
      val cols = line.split(";")
      if (cols.length > 2) {
        methodService.create(conferenceId, cols(0), cols(1).toInt, cols(2))
      } else {
        methodService.create(conferenceId, cols(0), 0, cols(1))
      }
    })
    Source.fromFile("statterms/templates/" + templateName + "/assumptions.csv", "UTF-8").getLines().foreach(line => {
      val cols = line.split(";")
      if (cols.length > 1) {
        assumptionService.create(conferenceId, cols(0), cols(1))
      } else {
        assumptionService.create(conferenceId, cols(0), "")
      }
    })
    Source.fromFile("statterms/templates/" + templateName + "/met2ass.csv", "UTF-8").getLines().foreach(line => {
      val cols = line.split(";")
      val methodId = methodService.findByName(conferenceId, cols(0)).get.id.get
      val assumptionId = assumptionService.findByName(conferenceId, cols(1)).get.id.get
      if (cols.length > 2) {
        method2AssumptionService.create(conferenceId, methodId, assumptionId, cols(2))
      } else {
        method2AssumptionService.create(conferenceId, methodId, assumptionId, "")
      }
    })

  }

  def conferenceEditor(conferenceId: Int, secret: String = "") = Action {
    val conference = conferenceService.findByIdAndSecret(conferenceId, secret)
    if (conference.isEmpty) {
      Unauthorized(views.html.error.unauthorized())
    } else {
      val papers = papersService.findByConference(conferenceId)
      val papersWithStats = PaperStats.getStats(papers, papersService, paperResultService,
        answerService, conferenceSettingsService)
      val stats = calculateConferenceStats(conferenceId, papersWithStats)
      Ok(views.html.conference.conferenceEditor(conferenceId, secret, conference.get.name, stats, papersWithStats))
    }
  }

  def calculateConferenceStats(conferenceId: Int, papersWithStats: List[PapersWithStats]): Map[String, String] = {
    var paperWithWarnings = 0
    var paperWithErrors = 0
    var paperWithWarningOrError = 0
    var paperWarningErrorByType: Map[String, Int] = Map()

    papersWithStats.foreach(p => {
      if (p.statsTotal.getOrElse(PaperResult.SYMBOL_ERROR, 0) > 0) paperWithErrors += 1
      if (p.statsTotal.getOrElse(PaperResult.SYMBOL_WARNING, 0) > 0) paperWithWarnings += 1
      if (p.statsTotal.getOrElse(PaperResult.SYMBOL_WARNING, 0) > 0 || p.statsTotal.getOrElse(PaperResult.SYMBOL_ERROR, 0) > 0) {
        paperWithWarningOrError += 1
      }
      p.statDetails.foreach(sd => {
        paperWarningErrorByType += (sd._1 -> (paperWarningErrorByType.getOrElse(sd._1, 0) + sd._2))
      })
    })

    var stats: Map[String, String] = Map()
    stats += ("paperTotal" -> papersService.countPapersByConference(conferenceId).toString)
    stats += ("paperWithWarnings" -> paperWithWarnings.toString)
    stats += ("paperWithErrors" -> paperWithErrors.toString)
    stats += ("paperWithWarningsOrErrors" -> paperWithWarningOrError.toString)
    stats += ("methodsTotal" -> paperMethodService.countByConferenceTotal(conferenceId).toString)
    stats += ("paperWithMethods" -> paperMethodService.countByConferencePapers(conferenceId).toString)
    stats += ("m2ATotal" -> answerService.countAnswersByConferenceTotal(conferenceId).toString)
    stats += ("paperWithM2A" -> answerService.countAnswersByConferencePaper(conferenceId).toString)
    paperWarningErrorByType.foreach(p => {
      stats += (p._1 -> p._2.toString)
    })
    stats
  }

  def flagEditor(conferenceId: Int, secret: String) = Action {
    val conference = conferenceService.findByIdAndSecret(conferenceId, secret)
    if (conference.isEmpty) {
      Unauthorized(views.html.error.unauthorized())
    } else {
      Ok(views.html.conference.conferenceFlags(conferenceId, secret, conference.get.name, conferenceSettingsService.findAllByConference(conferenceId)))
    }
  }

  def saveFlags = Action(parse.json) { request =>
    request.body.asOpt[Map[String, String]].map { cs =>
      val conference = conferenceService.findByIdAndSecret(cs("conferenceId").toInt, cs("secret"))
      if (conference.isEmpty) Unauthorized(views.html.error.unauthorized())
      if (cs("settingId").toInt < 0) {
        conferenceSettingsService.create(cs("conferenceId").toInt, cs("m2aId").toInt, cs("flag").toInt)
      } else {
        conferenceSettingsService.update(cs("settingId").toInt, cs("conferenceId").toInt, cs("flag").toInt)
      }
      Ok("Ok")
    }.getOrElse {
      Ok("Error")
    }
  }

  def getMethodsCSV(conferenceId: Int, secret: String) = Action {
    val conference = conferenceService.findByIdAndSecret(conferenceId, secret)
    if (conference.isEmpty) {
      Unauthorized(views.html.error.unauthorized())
    } else {
      val paperMethodsCSV = paperMethodService.getByConference(conferenceId)
      val csvFile = new File(PreprocessPDF.TMP_DIR + "/methods-" + conferenceId + "-" + secret + ".csv")
      val pw = new PrintWriter(csvFile)
      paperMethodsCSV.foreach(pm => {
        pw.write("\"" + pm.name + "\",\"" + pm.method + "," + pm.pos + "\"\r\n")
      })
      pw.close()
      Ok.sendFile(csvFile).withHeaders(
        "Content-Disposition" -> "attachment;filename=methods.csv"
      )
    }
  }

  def getPairsCSV(conferenceId: Int, secret: String) = Action {
    val conference = conferenceService.findByIdAndSecret(conferenceId, secret)
    if (conference.isEmpty) {
      Unauthorized(views.html.error.unauthorized())
    } else {
      val paperPairsCSV = answerService.findByConferenceId(conferenceId)
      val csvFile = new File(PreprocessPDF.TMP_DIR + "/pairs-" + conferenceId + "-" + secret + ".csv")
      val csvWriter = CSVWriter.open(csvFile)
      paperPairsCSV.foreach(pp => {
        val parsedGroup = pp.groupName.split("/")
        val pair = "\"" + pp.method.replaceAll("_", ",") + "," + parsedGroup(1) + "\",\"" + parsedGroup(2) + "\""
        csvWriter.writeRow(List(parsedGroup(0), pair, pp.isValid >= 0.5))
      })
      csvWriter.close()
      Ok.sendFile(csvFile).withHeaders(
        "Content-Disposition" -> "attachment;filename=pairs.csv"
      )
    }
  }

  def getAllPermutationsCSV = Action {
    def extractMethodName(methodIndex: String) = methodIndex.substring(0, methodIndex.indexOf("_"))

    def extractAssumptionName(groupName: String) = groupName.split("/").apply(1)

    def isPermutationCheckedInPaper(state: Long) = if (state < 0) 0 else 1

    DBSettings.initialize()
    val dao = new BallotDAO()
    val allPapers = papersService.findAll().map(p => p.id.get.toLong -> p).toMap
    val allConferences = conferenceService.findAll().map(c => c.id.get -> c).toMap
    val header = List("conference", "paper id", "paper name", "method name", "assumption name", "permutation checked", "group name", "method index", "actual state", "excluded in step id")
    val data = dao.getAllPermutations().map(p => {
      val paper = allPapers(p.paperId)
      val conferenceName = allConferences(paper.conferenceId).name
      List(conferenceName, p.paperId, paper.name, extractMethodName(p.methodIndex), extractAssumptionName(p.groupName), isPermutationCheckedInPaper(p.state).toString, p.groupName, p.methodIndex, p.state, p.excluded_step)
    })
    val csvFile = File.createTempFile("permutations", ".csv")
    val csvWriter = CSVWriter.open(csvFile)
    csvWriter.writeAll(header :: data)
    csvWriter.close()
    Ok.sendFile(csvFile).withHeaders("Content-Disposition" -> "attachment;filename=permutations.csv")
  }
}
