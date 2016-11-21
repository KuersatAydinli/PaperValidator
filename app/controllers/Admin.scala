package controllers

/**
  * Created by Aydinli on 21.11.2016.
  */
import java.io._
import java.util.zip.{ZipEntry, ZipInputStream}
import javax.inject.Inject

import helper.pdfpreprocessing.PreprocessPDF
import helper.{Commons, PaperProcessingManager}
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
    Ok(views.html.admin(conferences))
  }
}
