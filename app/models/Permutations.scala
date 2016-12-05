package models

import javax.inject.Inject

import anorm.JodaParameterMetaData._
import anorm.SqlParser._
import anorm._
import org.joda.time.DateTime
import play.api.db.Database


class PermutationsService @Inject()(db: Database) {
  def create(groupName: String, methodIndex: String, paperId: Int): Long =
    db.withConnection { implicit c =>
      SQL("INSERT INTO permutations(create_time, group_name, method_index, snippet_filename,pdf_path, method_on_top, " +
        "state, excluded_step, relative_height_top, relative_height_bottom, distanceMinIndexMax, paper_id) " +
        "VALUES ({create_time},{group_name},{method_index},'','',false,0,2,0.0,0.0,0,{paper_id})").on(
        'create_time -> DateTime.now(),
        'group_name -> groupName,
        'method_index -> methodIndex,
        'paper_id -> paperId
      ).executeInsert(scalar[Long].single)
    }
}