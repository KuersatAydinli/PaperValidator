package controllers

/**
  * Created by Aydinli on 21.11.2016.
  */
import javax.inject.Inject

import helper.PaperProcessingManager
import models._
import play.api.db.Database
import play.api.mvc.{Action, Controller}
import play.api.{Configuration, Logger}


class Admin @Inject()(database: Database, configuration: Configuration, questionService: QuestionService,
                      papersService: PapersService, conferenceService: ConferenceService,
                      method2AssumptionService: Method2AssumptionService, paperResultService: PaperResultService,
                      paperMethodService: PaperMethodService, permutationsServcie: PermutationsService,
                      answerService: AnswerService, conferenceSettingsService: ConferenceSettingsService
                     ) extends Controller{

  def show_admin = Action{
    PaperProcessingManager.run(database, configuration, papersService, questionService, method2AssumptionService,
      paperResultService, paperMethodService, permutationsServcie, answerService, conferenceSettingsService)
    val conferences = conferenceService.findAll()
    val papers = papersService.findByConference(1)
    val papersWithStats = PaperStats.getStats(papers, papersService, paperResultService,
      answerService, conferenceSettingsService)
    val conferenceIds = List[Int]()
    val conIdPaperDict = Map[Int,List[PapersWithStats]]()

//    for(conference <- conferenceService.findAll()){
//      conferenceIds.++(conference.id)
//    }
//
//    for(id <- conferenceIds){
//      conIdPaperDict.+ (id -> PaperStats.getStats(papersService.findByConference(id),
//        papersService, paperResultService,
//        answerService, conferenceSettingsService))
//    }

    conIdPaperDict foreach ( (t2) => Logger.info(t2._1 + "-->" + t2._2))

//    Ok(views.html.admin(conIdPaperDict,conferences, conferenceIds))
    Ok(views.html.admin(configuration, paperResultService, answerService, conferenceSettingsService, papersService, conferences, papersWithStats))
  }

  def long(a: AnyRef) :Long = {
    a match {
      case v: java.lang.Integer => v.intValue
      case v: java.lang.Long => v.longValue
    }
  }
}
