package helper.questiongenerator

import ch.uzh.ifi.pdeboer.pplib.hcomp.ballot.persistence.Answer
import ch.uzh.ifi.pdeboer.pplib.hcomp.{HCompAnswer, HCompPortalAdapter, HCompQuery, HCompQueryProperties}
import play.api.Logger

/**
  * Created by pdeboer on 16.01.17.
  */
class StopResumePortal(decorated: HCompPortalAdapter, private var existingAnswers: List[Answer]) extends HCompPortalAdapter {
  override def processQuery(query: HCompQuery, properties: HCompQueryProperties): Option[HCompAnswer] = {
    if (existingAnswers.nonEmpty) {
      val theAnswer = existingAnswers.head
      existingAnswers = existingAnswers.tail
      Logger.info(s"found answer in DB. returning it: $theAnswer")
      Some(theAnswer.toHTMLAnswer)
    } else decorated.processQuery(query, properties)
  }

  override def getDefaultPortalKey: String = "decorated_" + decorated.getDefaultPortalKey

  override def cancelQuery(query: HCompQuery): Unit = decorated.cancelQuery(query)
}
