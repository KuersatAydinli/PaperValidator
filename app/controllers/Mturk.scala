package controllers


import java.security.SecureRandom
import javax.inject.Inject

import helper.QuestionHTMLFormatter
import models._
import org.joda.time.DateTime
import play.Configuration
import play.api.Logger
import play.api.mvc._

import scala.util.parsing.json.JSONObject

object Mturk {
  val TEMPLATE_ID = 1L
  val TURKER_ID_KEY: String = "TurkerId"
}

class Mturk @Inject()(configuration: Configuration, questionService: QuestionService, answerService: AnswerService, assetService: AssetService, userService: UserService, batchService: BatchService, log: Log) extends Controller {

  def showAsset(id: Long, secret: String) = Action { request =>
    val parentQuestions = questionService.findByAssetId(id).filter(_.secret == secret)
    val turkerId: Option[String] = sessionUser(request)

    val isAssetOfTemplate: Boolean = parentQuestions.exists(_.id.get == Mturk.TEMPLATE_ID)
    if (!isAssetOfTemplate && logAccessAndCheckIfExceedsAccessCount(request, turkerId.orNull)) {
      Unauthorized("We received too many requests by your IP address")
    } else {

      if (turkerId.isDefined || isAssetOfTemplate) {
        val asset = assetService.findById(id)
        val hasUnansweredQuestions: Boolean = !parentQuestions.forall(q => answerService.existsAcceptedAnswerForQuestionId(q.id.get))

        if (asset.isDefined && parentQuestions.nonEmpty && hasUnansweredQuestions) {
          val contentType = asset.get.contentType
          if (contentType.equalsIgnoreCase("application/pdf")) {
            Ok(asset.get.byteArray).as(contentType)
          } else {
            Ok(asset.get.byteArray)
          }
        } else {
          UnprocessableEntity("There exists no asset with id: " + id)
        }
      } else {
        Ok(views.html.login())
      }
    }
  }

  def sessionUser(request: Request[AnyContent]): Option[String] = request.session.get(Mturk.TURKER_ID_KEY).filterNot(_.isEmpty)

  def showMTQuestion(uuid: String, secret: String, assignmentId: String, hitId: String, turkSubmitTo: String, workerId: String, target: String) = Action { request =>
    if (workerId.length > 2 && userService.findByTurkerId(workerId).isEmpty) {
      userService.create(workerId, new DateTime())
    }

    if (assignmentId == "ASSIGNMENT_ID_NOT_AVAILABLE") {
      def showAlreadyUsedMessage: Boolean = {
        if (sessionUser(request).isDefined) {
          val userFound = userService.findByTurkerId(sessionUser(request).get)
          if (userFound.isDefined) {
            val question = questionService.findById(questionService.findIdByUUID(uuid))
            if (question.isDefined) !checkUserDidntExceedMaxAnswersPerBatch(userFound.get.id.get, question.get) else false
          } else false
        } else false
      }

      if (showAlreadyUsedMessage) Unauthorized(views.html.tooManyAnswersInBatch(true)) else Ok(views.html.question(workerId, questionService.findById(Mturk.TEMPLATE_ID).map(q => new QuestionHTMLFormatter(q.html).format).getOrElse("No Example page defined")))

    } else {
      assert(!workerId.isEmpty)
      //val newSession = request.session + ("TurkerID" -> workerId) + ("assignmentId" -> assignmentId) + ("target" -> target)
      val newSession = request.session + (Mturk.TURKER_ID_KEY -> workerId) + ("assignmentId" -> assignmentId) + ("target" -> target)

      showQuestionAction(uuid, secret, request, workerId, Some(newSession))
    }
  }

  /**
    * Show a question.
    * The method below counts the number of answers already sent by the turker and loads the question
    * from the database.
    *
    * If this number is equal to the maximal number of allowed answer per turker per batch, then the turker will be
    * redirected to a warning page.
    *
    * @param uuid
    * @return
    */
  def showQuestion(uuid: String, secret: String = "") = Action { request =>
    showQuestionAction(uuid, secret, request, request.session.get(Mturk.TURKER_ID_KEY).get)
  }

  def showQuestionAction(uuid: String, secret: String, request: Request[AnyContent], turkerId: String, _replaceSession: Option[Session] = None) = {
    //dont allow session to reset turker ID field
    val replaceSession = _replaceSession.map(s => {
      s + (Mturk.TURKER_ID_KEY -> turkerId)
    }).getOrElse(request.session)

    if (!logAccessAndCheckIfExceedsAccessCount(request, turkerId)) {
      val questionId = questionService.findIdByUUID(uuid)
      if (questionId == Mturk.TEMPLATE_ID) Logger.debug(s"user tried to look at template after being accepted: $turkerId")
      assert(questionId != Mturk.TEMPLATE_ID, "you are about to look at the template. This should not happen")

      val userFound = userService.findByTurkerId(turkerId)
      if (userFound.isDefined) {
        if (checkUserDidntExceedMaxAnswersPerBatch(userFound.get.id.get, questionService.findById(questionId).get)) {
          Unauthorized(views.html.tooManyAnswersInBatch()).withSession(replaceSession)
        } else if (isUserAllowedToAnswer(questionId, userFound.get.id.get, secret)) {
          val question = questionService.findById(questionId).get
          val formattedHTML: String = new QuestionHTMLFormatter(question.html).format
          Ok(views.html.question(turkerId, formattedHTML, questionId, secret)).withSession(replaceSession)
        } else {
          Unauthorized("This HIT has already been answered / you don't have permission to answer this HIT. If this error persists, please write pdeboer@mit.edu ")
        }
      } else {
        Logger.debug(s"can't find user. Asking to log in: $userFound, $turkerId")
        Ok(views.html.login()).withSession(replaceSession + "redirect" -> (configuration.getString("url.prefix") + "/showQuestion?q=" + uuid + "&s=" + secret))
      }
    } else Unauthorized("We have received too many requests from your IP address")
  }


  def insertSnippetInHTMLPage(html: String, snippet: String): String = {
    html.replace("<img id=\"snippet\" src=\"\" width=\"100%\"></img>", "<img id=\"snippet\" src=\"data:image/gif;base64," + snippet + "\" width=\"100%\"></img>")
  }

  def removeCDATA(html: String): String = {
    html.replaceAll("\\Q<![CDATA[\\E", "").replaceAll("\\Q]]>\\E", "")
  }

  def isUserAllowedToAnswer(questionId: Long, userId: Long, providedSecret: String = ""): Boolean = {
    val question = questionService.findById(questionId)
    // The question exists and there is no answer yet accepted in the DB
    question.isDefined && !answerService.existsAcceptedAnswerForQuestionId(questionId) && question.get.secret == providedSecret
  }

  def checkUserDidntExceedMaxAnswersPerBatch(userId: Long, question: Question): Boolean = {
    val batch = batchService.findById(question.batchId)
    if (batch.get.allowedAnswersPerTurker == 0) {
      true
    } else {
      if (batch.get.allowedAnswersPerTurker > answerService.countUserAnswersForBatch(userId, question.batchId)) {
        true
      } else {
        false
      }
    }
  }

  /**
    * Store an answer in the database.
    * This method extract the answer of a question and stores it in the database. After storing the answer the user will
    * be redirected to a conclusion page where a code is displayed in order to get the reward.
    *
    * @return
    */
  def storeAnswer = Action { request =>
    request.session.get(Mturk.TURKER_ID_KEY).map { user =>
      try {

        val questionId = request.getQueryString("questionId").mkString.toLong
        val isRelated = request.getQueryString("isRelated").mkString == "Yes"
        val isCheckedBefore = request.getQueryString("isCheckedBefore").mkString == "Yes"
        val confidence = request.getQueryString("confidence").mkString.toInt
        val extraAnswer = request.getQueryString("extraAnswer").mkString == "Yes"
        val secret = request.getQueryString("secret").mkString
        val userId: Long = userService.findByTurkerId(user).get.id.get

        if (isUserAllowedToAnswer(questionId, userId, secret)) {
          val outputCode = Math.abs(new SecureRandom().nextLong())

          val answer: JSONObject = JSONObject.apply(request.queryString.map(m => {
            (m._1, m._2.mkString(","))
          }))

          answerService.create(questionId, userId, new DateTime, isRelated, isCheckedBefore, extraAnswer, confidence, answer.toString(), outputCode)

          if (request.session.get("assignmentId").isDefined) {
            val newSessionInclUser: Session = Session() + (Mturk.TURKER_ID_KEY -> request.session.get(Mturk.TURKER_ID_KEY).get)
            Ok(views.html.postToTurk(request.session.get("target").get, request.session.get("assignmentId").get, outputCode)).withSession(newSessionInclUser)
          } else
            Ok(views.html.code(user, outputCode)).withSession(request.session)
        } else {
          Logger.debug(s"$userId was not allowed to answer since the question has already been answered")
          Unauthorized("This question has already been answered.")
        }
      } catch {
        case e: Exception => {
          e.printStackTrace()
          Unauthorized("Invalid request format.")
        }
      }
    }.getOrElse {
      Ok(views.html.login())
    }
  }

  def logAccessAndCheckIfExceedsAccessCount(request: Request[AnyContent], username: String = ""): Boolean = {
    val userIdCleaned = userService.findByTurkerId(username).map(_.id.get).getOrElse(-1L)

    log.createEntry(request.uri, request.remoteAddress, userIdCleaned)

    val requestsPerSnippetAnswer = 3
    val maxSnippetsPerCrowdWorker: Int = 5000

    val numberOfEntries = log.ipLogEntriesSince(request.remoteAddress, DateTime.now().minusWeeks(4))
    if (numberOfEntries.isLeft) {
      Logger.debug("logAccessAndCheckIfExceedsAccessCount error")
      true
    } else {
      numberOfEntries.right.get > (requestsPerSnippetAnswer * maxSnippetsPerCrowdWorker)
    }
  }

}