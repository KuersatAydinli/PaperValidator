package ch.uzh.ifi.pdeboer.pplib.hcomp.ballot

import java.io.File
import javax.activation.MimetypesFileTypeMap

import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.dao.DAO
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.integrationtest.console.Constants
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.persistence.{Permutation, PermutationState}
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.report.{JsonAnswerParser, ParsedAnswer}
import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.snippet.{SnippetHTMLQueryBuilder, SnippetHTMLTemplate}
import ch.uzh.ifi.pdeboer.pplib.hcomp.{HCompPortalAdapter, HCompQueryProperties, HTMLQueryAnswer}
import ch.uzh.ifi.pdeboer.pplib.process.entities.DefaultParameters._
import ch.uzh.ifi.pdeboer.pplib.process.entities.IndexedPatch
import ch.uzh.ifi.pdeboer.pplib.process.stdlib.ContestWithBeatByKVotingProcess
import ch.uzh.ifi.pdeboer.pplib.process.stdlib.ContestWithBeatByKVotingProcess._
import helper.questiongenerator.StopResumePortal
import models.Method2AssumptionService
import play.Configuration
import play.api.Logger

import scala.xml.NodeSeq


/**
  * Created by mattia on 01.09.15.
  */
case class Algorithm250(dao: DAO, ballotPortalAdapter: HCompPortalAdapter, method2AssumptionService: Method2AssumptionService) {

  val ALLOW_SAME_ASSUMPTION_TO_BE_REPORTED_FOR_MULTIPLE_METHODS = true

  val conf: Configuration = Configuration.root()

  def executePermutation(p: Permutation): Unit = {
    val answers: List[ParsedAnswer] = buildAndExecuteQuestion(p)
    Logger.info(s"got answer for question ${p.id}")
    if (isFinalAnswerYesYes(answers)) {
      dao.updateStateOfPermutationId(p.id, p.id) //set done
      if (ALLOW_SAME_ASSUMPTION_TO_BE_REPORTED_FOR_MULTIPLE_METHODS) {
        dao.getAllOpenByGroupName(p.groupName).foreach(g => {
          //same assumption (and index) can only be used for 1 method)
          dao.updateStateOfPermutationId(g.id, p.id, PermutationState.EXCLUDED_OTHER_METHOD_MATCHES_FOR_THIS_ASSUMPTION)
        })
      }
      val groupName = p.groupName.split("/")
      val secondExclusionMatches = groupName.slice(0, 2).mkString("/") //equal assumption (e.g. normal for ANOVA)
      dao.getAllOpenGroupsStartingWith(secondExclusionMatches).filter(_.methodIndex.equalsIgnoreCase(p.methodIndex)).foreach(g => {
        //deactivate all other "normal" checks for ANOVA if we just accepted NORMAL to be ok for that anova
        dao.updateStateOfPermutationId(g.id, p.id, PermutationState.EXCLUDED_SAME_ASSUMPTIONS_FOR_METHOD)
      })
    } else {
      dao.updateStateOfPermutationId(p.id, PermutationState.ANSWER_IS_NO)
    }
  }


  def buildAndExecuteQuestion(permutation: Permutation): List[ParsedAnswer] = {
    val (properties: BallotProperties, ballotHtmlPage: NodeSeq) = buildQuestion(permutation)
    val description: String = "Can you grasp some of the main concepts in the field of statistics without necessary prior knowledge in the field - just by basic text understanding?"
    val title: String = s"Are these two words related? {Batch ${properties.permutationId}}"
    Logger.debug(s"running BBK on $permutation")
    val process = new ContestWithBeatByKVotingProcess(Map(
      K.key -> conf.getInt("hcomp.k"),
      PORTAL_PARAMETER.key -> new StopResumePortal(ballotPortalAdapter, dao.getAnswersForPermutation(permutation.id)),
      MAX_ITERATIONS.key -> conf.getInt("hcomp.maxIterations"),
      MEMOIZER_NAME.key -> Some("bbk_mem_" + properties.permutationId),
      QUESTION_PRICE.key -> properties,
      QUERY_BUILDER_KEY -> new SnippetHTMLQueryBuilder(ballotHtmlPage, description, title)
    ))

    process.process(IndexedPatch.from(List(SnippetHTMLQueryBuilder.POSITIVE, SnippetHTMLQueryBuilder.NEGATIVE)))

    process.portal.queries.map(_.answer.get.is[HTMLQueryAnswer]).map(a => {
      ParsedAnswer(a.answers.get("isRelated"), a.answers.get("isCheckedBefore"), a.answers("confidence").toInt, a.answers("descriptionIsRelated"))
    })
  }

  def buildQuestion(permutation: Permutation, isTemplate: Boolean = false): (BallotProperties, NodeSeq) = {
    val snippetFile: File = new File(permutation.snippetFilename)
    import java.nio.file.{Files, Paths}

    val snippetByteArray = try {
      Files.readAllBytes(Paths.get(permutation.snippetFilename))
    } catch {
      case e: Throwable => Logger.error("problem reading snippet", e); throw e
    }

    val javascriptByteArray = if (permutation.methodOnTop) {
      SnippetHTMLTemplate.generateJavascript.toString.map(_.toByte).toArray
    } else {
      SnippetHTMLTemplate.generateJavascript.toString.map(_.toByte).toArray
    }

    val snippetContentType = new MimetypesFileTypeMap().getContentType(snippetFile.getName)
    val javascriptContentType = "application/javascript"

    val snippetAsset = Asset(snippetByteArray, snippetContentType, snippetFile.getName)
    val jsAsset = Asset(javascriptByteArray, javascriptContentType, "script.js")

    val qualifications = if (conf.getBoolean("hcomp.qualifications", true)) HCompQueryProperties.DEFAULT_QUALIFICATIONS else Nil
    if (qualifications.isEmpty) Logger.debug("watch out, no qualifications set for MTurk")
    val properties = new BallotProperties(dao.getBatchByPermutation(permutation.id).getOrElse(Batch(allowedAnswersPerTurker = 1)), List(
      snippetAsset, jsAsset), permutation.id, propertiesForDecoratedPortal = new HCompQueryProperties(conf.getInt("hcomp.paymentCents"), qualifications = qualifications)) //TODO put in qualifications

    //val method = permutation.methodIndex.substring(0,permutation.methodIndex.indexOf("_")).toLowerCase()
    //val assumption = permutation.groupName.substring(permutation.groupName.indexOf("/")+1,permutation.groupName.lastIndexOf("/")).toLowerCase()
    //val m2a = method2AssumptionService.findByMethodAndAssumptionName(method,assumption,conferenceId).get
    val ballotHtmlPage: NodeSeq = SnippetHTMLTemplate.generateHTMLPage(snippetAsset.url, jsAsset.url, "", isTemplate)
    //SnippetHTMLTemplate.generateHTMLPage(snippetAsset.url, jsAsset.url, m2a.question, isTemplate)
    (properties, ballotHtmlPage)
  }

  def isFinalAnswerYesYes(answers: List[ParsedAnswer]): Boolean = {
    val cleanedAnswers = answers.filter(_.likert >= Constants.LIKERT_VALUE_CLEANED_ANSWERS)
    val yesYes = cleanedAnswers.count(ans => JsonAnswerParser.evaluateAnswer(ans.q1).contains(true) && JsonAnswerParser.evaluateAnswer(ans.q2).contains(true))
    val no = cleanedAnswers.size - yesYes
    yesYes > no
  }
}
