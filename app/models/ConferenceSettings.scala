package models

import javax.inject.{Inject, Singleton}

import anorm.SqlParser._
import anorm._
import play.api.db.Database

/**
  * Created by manuel on 19.04.16.
  */
object ConferenceSettings {
  val FLAG_REQUIRE = 2
  val FLAG_EXPECT = 1
  val FLAG_IGNORE = 0
}

case class ConferenceSettings(id: Option[Int], method2AssumptionId: Int, methodName: String, assumptionName: String, flag: Option[Int]) extends Serializable

class ConferenceSettingsService @Inject()(db: Database) {

  private val answerParser: RowParser[ConferenceSettings] =
    get[Option[Int]]("id") ~
      get[Int]("method2assumption_id") ~
      get[String]("method_name") ~
      get[String]("assumption_name") ~
      get[Option[Int]]("flag") map {
      case id ~ method2assumption_id ~ method_name ~ assumption_name ~ flag =>
        ConferenceSettings(id, method2assumption_id, method_name, assumption_name, flag)
    }

  def findById(id: Int, conferenceId: Int): Option[ConferenceSettings] =
    db.withConnection { implicit c =>
      SQL("SELECT cs.id, m2a.id methods2assumption_id, m2a.method_name, m2a.assumption_name, cs.flag " +
        "FROM (SELECT m2a.id, m2a.method_id, m.name method_name, m2a.assumption_id, a.name assumption_name " +
        "FROM methods2assumptions m2a,methods m,assumptions a " +
        "WHERE m2a.method_id=m.id AND m2a.assumption_id=a.id AND m2a.conference_id={conference_id} " +
        "AND m.conference_id={conference_id} AND a.conference_id={conference_id} ) m2a " +
        "LEFT JOIN conference_settings cs ON m2a.id = cs.method2assumption_id " +
        "AND cs.conference_id={conference_id} AND cs.id = {id}").on(
        'id -> id,
        'conference_id -> conferenceId
      ).as(answerParser.singleOpt)
    }

  def findAllByConference(conferenceId: Int): List[ConferenceSettings] = {
    db.withConnection { implicit c =>
      SQL("SELECT cs.id, m2a.m2aid method2assumption_id, m2a.method_name, m2a.assumption_name, IFNULL(cs.flag,1) flag " +
        "FROM (SELECT m2a.id m2aid, m2a.method_id, m.name method_name, m2a.assumption_id, a.name assumption_name " +
        "FROM methods2assumptions m2a,methods m,assumptions a " +
        "WHERE m2a.method_id=m.id AND m2a.assumption_id=a.id AND m2a.conference_id={conference_id} " +
        "AND m.conference_id={conference_id} AND a.conference_id={conference_id}) m2a " +
        "LEFT JOIN conference_settings cs ON m2a.m2aid = cs.method2assumption_id AND cs.conference_id={conference_id} " +
        "ORDER BY method_name ASC, assumption_name ASC").on(
        'conference_id -> conferenceId
      ).as(answerParser *)
    }
  }

  def findAllByPaperId(paperId: Int, conferenceId: Int): List[ConferenceSettings] = {
    db.withConnection { implicit c =>
      SQL("SELECT * FROM " +
        "(SELECT cs.id, m2a.m2aid method2assumption_id, m2a.method_name, m2a.assumption_name, IFNULL(cs.flag,1) flag " +
        "FROM (SELECT m2a.id m2aid, m2a.method_id, m.name method_name, m2a.assumption_id, a.name assumption_name " +
        "FROM methods2assumptions m2a,methods m,assumptions a " +
        "WHERE m2a.method_id=m.id AND m2a.assumption_id=a.id AND m2a.conference_id={conference_id} " +
        "AND m.conference_id={conference_id} AND a.conference_id={conference_id}) m2a " +
        "LEFT JOIN conference_settings cs ON m2a.m2aid = cs.method2assumption_id AND cs.conference_id={conference_id} " +
        "ORDER BY method_name ASC, assumption_name ASC) conf " +
        "WHERE method_name IN (SELECT method FROM paper_methods WHERE paper_id = {paper_id} GROUP BY method)").on(
        'paper_id -> paperId,
        'conference_id -> conferenceId
      ).as(answerParser *)
    }
  }

  def create(conferenceId: Int, method2AssumptionId: Int, flag: Int) =
    db.withConnection { implicit c =>
      SQL("INSERT INTO conference_settings(conference_id, method2assumption_id, flag) " +
        "VALUES ({conference_id}, {method2assumption_id}, {flag})").on(
        'conference_id -> conferenceId,
        'method2assumption_id -> method2AssumptionId,
        'flag -> flag
      ).executeInsert()
    }

  def update(id: Int, conferenceId: Int, flag: Int) =
    db.withConnection { implicit c =>
      SQL("UPDATE conference_settings SET flag={flag} WHERE id={id} AND conference_id={conference_id}").on(
        'id -> id,
        'conference_id -> conferenceId,
        'flag -> flag
      ).executeUpdate()
    }

  def deleteByMethodId(methodId: Int) =
    db.withConnection { implicit c =>
      SQL("DELETE FROM conference_settings WHERE method2assumption_id IN " +
        "(SELECT * FROM (SELECT m2a.id FROM conference_settings cs, methods2assumptions m2a " +
        "WHERE cs.method2assumption_id = m2a.id AND m2a.method_id = {method_id}) ids)").on(
        'method_id -> methodId
      ).executeUpdate()
    }

  def deleteByAssumptionId(assumptionId: Int) =
    db.withConnection { implicit c =>
      SQL("DELETE FROM conference_settings WHERE method2assumption_id IN " +
        "(SELECT * FROM (SELECT m2a.id FROM conference_settings cs, methods2assumptions m2a " +
        "WHERE cs.method2assumption_id = m2a.id AND m2a.assumption_id = {assumption_id}) ids)").on(
        'assumption_id -> assumptionId
      ).executeUpdate()
    }


  def deleteByM2AId(m2aId: Int) =
    db.withConnection { implicit c =>
      SQL("DELETE FROM conference_settings WHERE method2assumption_id = {m2aId}").on(
        'm2aId -> m2aId
      ).executeUpdate()
    }

  def delete(id: Int, conferenceId: Int) =
    db.withConnection { implicit c =>
      SQL("DELETE FROM conference_settings WHERE id={id} AND conference_id={conference_id}").on(
        'id -> id,
        'conference_id -> conferenceId
      ).executeUpdate()
    }

}