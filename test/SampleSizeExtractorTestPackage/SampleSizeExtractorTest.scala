package SampleSizeExtractorTestPackage

import java.io._

import com.google.common.base.CharMatcher
import helper.pdfpreprocessing.pdf.PDFTextExtractor
import helper.pdfpreprocessing.pdf.entity.{Table => trapRangeTable}
import helper.statcheck.{Statchecker => StatChecker}
import org.apache.commons.io.FilenameUtils
//import org.python.core.PyInteger
//import org.python.util.PythonInterpreter
import org.scalatest.FunSuite

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks
import scala.util.matching.Regex

/**
  * Created by Aydinli on 26.01.2017.
  */
class SampleSizeExtractorTest extends FunSuite{

  test("Regex Testings"){
    val Label_Source = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes_copy_full_labels.csv")
    val gt_ol1 = mutable.ListBuffer.empty[Int]
    var gt_nl1 = mutable.ListBuffer.empty[Int]
    val gt_l2_helper = mutable.Map.empty[String,mutable.ListBuffer[Int]]
    val gt_l2 = mutable.ListBuffer.empty[mutable.ListBuffer[Int]]

    /*These Lists represent the different Patterns to match the corresponding sample size types*/
    val patternsInitialSampleSize = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,40}\\s*invited"),
      new Regex("invited\\s*\\d+([,\\s*]\\d{3})*\\D{0,20}"),
      new Regex("reviewed\\s*\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
        "persons)"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
        "persons)\\s*reviewed"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,40}\\s*screened"),
      new Regex("screened\\s*\\d+([,\\s*]\\d{3})*\\D{0,20}"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}\\s*assessed\\s*for\\s*eligibility"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}\\s*met\\s*\\D{0,20}\\s*criteria")
    )
    val patternsActualSampleSize = mutable.MutableList(
      new Regex("[Oo]f\\s*the\\s*\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,25}(recruited|included)"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,20}\\s*recruited"),
      new Regex("recruited\\s*\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,25}were\\s*randomized"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,25}randomly\\s*(allocated|assigned)\\s*to"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,35}underwent\\s*randomization"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,20}\\s*were\\s*included\\s*\\D{0,20}\\s*analysis"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
        "persons)\\s*\\D{0,20}\\s*included"),
      new Regex("included\\s*\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
        "persons)\\s*\\D{0,20}")
    )
    val patternsActualSampleSizeNegative = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,40}\\s*not\\D{0,15}\\s*eligible"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,40}\\s*(did\\s*not|didn't)\\s*meet\\D{0,15}\\s*(criteria|criterion)"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*(people|patients|participants|subjects|cases|persons)\\D{0,20}\\s*were\\s*excluded"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,20}\\s*were\\s*excluded")
    )
    val patternsGroupSampleSize = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*(people|patients|participants|subjects|cases|persons)?\\s*in\\s*the\\s*\\D{0,20}\\s*group"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*(people|patients|participants|subjects|cases|persons)?\\s*received"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,40}\\s*(allocated|assigned)\\s*to"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,10}\\s*controls"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\/\\s*\\d+([,\\s*]\\d{3})*")
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|persons)?\\s*of\\s*\\d+([,\\s*]\\d{3})*")
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*\\D{0,50}\\s*and\\s*\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?")
    )
    val patternsUsualKC = mutable.MutableList(
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\/\\s*\\d+([,\\s*]\\d{3})*"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*were\\s*assigned\\s*to"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}members"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}cases"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}controls"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}respondents"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}individuals"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}adults"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,25}recruited"),
      new Regex("\\s*[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("enrolled\\s*\\d+([,\\s*]\\d{3})*\\D{0,15}\\s*(people|patients|participants|subjects|cases|persons)?"),
      new Regex("\\s*data\\D{0,10}of\\D{0,5}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,10}from\\D{0,5}\\d+([,\\s*]\\d{3})*"))

    val list_tTestPapers = mutable.MutableList.empty[String]
    for(line <- Label_Source.getLines()){
//      index += 1
      val cols = line.split(",").map(_.trim)
      if(cols(6).equals("OL1")){
        gt_ol1 += cols(4).replaceAll("\\D+","").toInt
      }
      if(cols(6).equals("NL1")){
        gt_nl1 += cols(4).replaceAll("\\D+","").toInt
      }
      if(cols(6).equals("L2")){
        if(!gt_l2_helper.keySet.contains(cols(0))){
          gt_l2_helper += cols(0) -> mutable.ListBuffer.empty[Int]
        }
        gt_l2_helper(cols(0)) = gt_l2_helper(cols(0)) += cols(4).replaceAll("\\D+","").toInt
      }
      if(cols(6).contains("OL1") || cols(6).contains("NL1") || cols(6).contains("L2")){
        if(!list_tTestPapers.contains(cols(0))){
          list_tTestPapers += cols(0)
        }
      }
    }
    for(values <- gt_l2_helper.values){
      gt_l2 += values
    }

    val PdfPath = "test/TestPDFs"
    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
    val bracketList_NL1: List[String] = List("[","]",":","/","+","-",";","*")
    val bracketList_L2: List[String] = List("[","]",":","+","-",";","*")
//    val bracketList_OL1: List[String] = List("(",")","[","]",":","/","+",";","*")
    val files = getListOfFiles(PdfPath)
    var filesCount = 0
    val finalEvaluationList = mutable.Map.empty[String,mutable.Map[String,List[Int]]]

    for(file <- files){
      val SS_Pos_Map = mutable.ArrayBuffer.empty[Int]
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        for(ttestPaper <- list_tTestPapers){
          if(FilenameUtils.getBaseName(fileString).contains(ttestPaper.replace(".txt",""))){
            filesCount += 1
            val pdfText = convertPDFtoText(fileString)

            /*These lists represent the final sample size values to be returned for a paper*/
            val OL1_SS = mutable.ListBuffer.empty[Int] // initial sample size before any exclusions or allocations to study arms
            val NL1_SS = mutable.ListBuffer.empty[Int] // actual sample size on which the study was made (sum of study arm sample sizes)
            val L2_SS = mutable.ListBuffer.empty[Int] // group sample sizes of study arms/treatment groups if present


            /*These lists represent the different sample size matches including their integer pools*/
            val matchesInitialSS = mutable.ListBuffer.empty[String]
            val poolInitialSS = mutable.ListBuffer.empty[Int]
            val matchesActualSS = mutable.ListBuffer.empty[String]
            val poolActualSS = mutable.ListBuffer.empty[Int]
            val matchesActualSSNegative = mutable.ListBuffer.empty[String]
            val poolActualSSNegative = mutable.ListBuffer.empty[Int]
            val matchesGroupSS = mutable.ListBuffer.empty[String]
            var poolGroupSS = mutable.ListBuffer.empty[String]
            val matchesUsualKC = mutable.ListBuffer.empty[String]
            val poolUsualKC = mutable.ListBuffer.empty[Int]

            for(regex <- patternsInitialSampleSize){
              if(regex.findAllIn(pdfText.mkString).nonEmpty){
                val totalMatches = regex.findAllIn(pdfText.mkString).matchData
                while(totalMatches.hasNext){
                  val currentMatch = totalMatches.next().toString()
                  if(!(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*"))){
                    matchesInitialSS += currentMatch
                  }
                }
              }
            }
            for(regex <- patternsActualSampleSize){
              if(regex.findAllIn(pdfText.mkString).nonEmpty){
                val totalMatches = regex.findAllIn(pdfText.mkString).matchData
                while(totalMatches.hasNext){
                  val currentMatch = totalMatches.next().toString()
                  if(!(bracketList_NL1.exists(currentMatch.contains(_)) || currentMatch.contains("not") || currentMatch.matches("\\d+\\D{0,3}[,.]\\D*"))){
                    matchesActualSS += currentMatch
                  }
                }
              }
            }
            for(regex <- patternsActualSampleSizeNegative){
              if(regex.findAllIn(pdfText.mkString).nonEmpty){
                val totalMatches = regex.findAllIn(pdfText.mkString).matchData
                while(totalMatches.hasNext){
                  val currentMatch = totalMatches.next().toString()
                  if(!(currentMatch.contains("%") && currentMatch.matches("\\d+\\s*%\\s*\\D{0,60}"))){
                    matchesActualSSNegative += currentMatch
                  }
                }
              }
            }
            for(regex <- patternsGroupSampleSize){
              if(regex.findAllIn(pdfText.mkString).nonEmpty){
                val totalMatches = regex.findAllIn(pdfText.mkString).matchData
                val whileLoop = new Breaks
                while(totalMatches.hasNext){
                  whileLoop.breakable{
                    val currentMatch = totalMatches.next().toString()
                    if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\s*\\/\\s*\\d+([,\\s*]\\d{3})*") && currentMatch.split("\\/")(1).length > 6){
                      whileLoop.break()
                    }
                    if(!(bracketList_L2.exists(currentMatch.contains(_)) || (currentMatch.matches("\\d+([,\\s*]\\d{3})*\\s*\\/\\s*\\d+([,\\s*]\\d{3})*")
                      && currentMatch.split("\\/")(0).replaceAll("\\D+","").toInt >
                      currentMatch.split("\\/")(1).replaceAll("\\D+","").toInt) || (currentMatch.matches("\\d+([,\\s*]\\d{3})*\\s*of\\s*\\d+([,\\s*]\\d{3})*")
                      && currentMatch.split("of")(0).replaceAll("\\D+","").toInt >
                      currentMatch.split("of")(1).replaceAll("\\D+","").toInt) || currentMatch.matches("\\d+\\D{0,3}[,.]\\D*"))){
                      matchesGroupSS += currentMatch
                    }
                  }
                }

              }
            }
            for(regex <- patternsUsualKC){
              if(regex.findAllIn(pdfText.mkString).nonEmpty){
                val totalMatches = regex.findAllIn(pdfText.mkString).matchData
                while(totalMatches.hasNext){
                  val currentMatch = totalMatches.next().toString()
                  if(!(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*") || currentMatch.contains("%") ||
                    currentMatch.matches("\\d+\\D*[.]\\D*") || bracketList.exists(currentMatch.contains(_)) ||
                    currentMatch.matches("\\d+\\s*[-]\\D*") || currentMatch.matches("\\d+\\s*[,]\\D*") ||
                    currentMatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
                      "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
                      "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
                      "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*") || (currentMatch.matches("\\d+\\D*\\s+[Hh]ow\\s+\\D*|\\d\\D*s+[Aa]lthough\\D*|\\d+\\D*\\s+[Tt]he\\s+\\D*" +
                    "|\\d+\\D*\\s+[Mm]any\\s+\\D*|\\d+\\D*\\s+[Oo]ften\\s+\\D*|\\d+\\D*\\s+found\\s+\\D*|\\d+\\D*\\s+[Oo]n\\s+\\D*" +
                    "|\\d+\\D*[Aa]mong\\D*|\\d+\\D*[Ss]how\\D*|\\d+\\D*[Tt]han\\D*|\\d+\\D*[Pp]ercent\\D*|\\d+\\D*[Ww]hich\\D*" +
                    "|\\d+\\D*[Dd]uring\\D*|\\d+\\D*as\\s*well\\s*as\\D*|\\d+\\D*[Aa]nother\\D*|\\d+\\D*[Cc]haracteristics\\D*" +
                    "|\\d+\\D*[Aa]ttribute\\D*|\\d+\\D*[Mm]ost\\D*|\\d+\\D*\\s+[Bb]ut\\s+\\D*|\\d+\\D*[Aa]bout\\D*" +
                    "|\\d+\\D*[Ww]hether\\D*|\\d+\\D*Number\\D*") &&
                    !currentMatch.matches("\\d+\\D*\\s+[Oo]f\\s*the\\s+\\D*")) || (!CharMatcher.ASCII.matchesAllOf(currentMatch)))){
                    matchesUsualKC += currentMatch
                    SS_Pos_Map += currentMatch.replaceAll("\\D+","").toInt
                  }
//                  if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
//                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
//                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
//                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
//                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
//                    "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*")){
//                    matchesUsualKC -= currentMatch
//                  } else if(currentMatch.contains("%")){
//                    matchesUsualKC -= currentMatch
//                  } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
//                    matchesUsualKC -= currentMatch
//                  } else if(bracketList.exists(currentMatch.contains(_))){
//                    matchesUsualKC -= currentMatch
//                  } else if(currentMatch.matches("\\d+\\s*[-]\\D*")){
//                    matchesUsualKC -= currentMatch
//                  } else if(currentMatch.matches("\\d+\\s*[,]\\D*")){
//                    matchesUsualKC -= currentMatch
//                  } else if(currentMatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
//                    "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
//                    "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
//                    "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*")){
//                    matchesUsualKC -= currentMatch
//                  } else if(currentMatch.matches("\\d+\\D*\\s+[Hh]ow\\s+\\D*|\\d\\D*s+[Aa]lthough\\D*|\\d+\\D*\\s+[Tt]he\\s+\\D*" +
//                    "|\\d+\\D*\\s+[Mm]any\\s+\\D*|\\d+\\D*\\s+[Oo]ften\\s+\\D*|\\d+\\D*\\s+found\\s+\\D*|\\d+\\D*\\s+[Oo]n\\s+\\D*" +
//                    "|\\d+\\D*[Aa]mong\\D*|\\d+\\D*[Ss]how\\D*|\\d+\\D*[Tt]han\\D*|\\d+\\D*[Pp]ercent\\D*|\\d+\\D*[Ww]hich\\D*" +
//                    "|\\d+\\D*[Dd]uring\\D*|\\d+\\D*as\\s*well\\s*as\\D*|\\d+\\D*[Aa]nother\\D*|\\d+\\D*[Cc]haracteristics\\D*" +
//                    "|\\d+\\D*[Aa]ttribute\\D*|\\d+\\D*[Mm]ost\\D*|\\d+\\D*\\s+[Bb]ut\\s+\\D*|\\d+\\D*[Aa]bout\\D*" +
//                    "|\\d+\\D*[Ww]hether\\D*|\\d+\\D*Number\\D*") &&
//                    !currentMatch.matches("\\d+\\D*\\s+[Oo]f\\s*the\\s+\\D*")){
//                    matchesUsualKC -= currentMatch
//                  } else if(!CharMatcher.ASCII.matchesAllOf(currentMatch)){
//                    matchesUsualKC -= currentMatch
//                  } else {
//                    SS_Pos_Map += currentMatch -> position
//                    position += 1
//                  }
                }
              }
            }

            /*Populate sample size Pools with Integer Values*/
            matchesInitialSS.foreach(initialSS => poolInitialSS += initialSS.replaceAll("\\D+","").toInt)
            matchesUsualKC.foreach(usualSS => poolUsualKC += usualSS.replaceAll("\\D+","").toInt)
            matchesActualSS.foreach(actualSS => {
              if(actualSS.contains("%)")){
                poolActualSS += actualSS.split("\\(\\s*\\d+")(0).replaceAll("\\D+","").toInt
              } else {
                poolActualSS += actualSS.replaceAll("\\D+","").toInt
              }
            })
            matchesActualSSNegative.foreach(actualSSNeg => {
              if(actualSSNeg.contains("%)")){
                poolActualSSNegative += actualSSNeg.split("\\(\\s*\\d+")(0).replaceAll("\\D+","").toInt
              } else {
                poolActualSSNegative += actualSSNeg.replaceAll("\\D+","").toInt
              }
            })
            matchesGroupSS.foreach(groupSS => {
              if(groupSS.contains("%)")){
                poolGroupSS += groupSS.split("\\(\\s*\\d+")(0).replaceAll("\\D+","")
              } else if(groupSS.contains("/")){
                poolGroupSS += groupSS.split("\\/")(0).replaceAll("\\D+","")+","+groupSS.split("\\/")(1).replaceAll("\\D+","")
              } else if(groupSS.contains("of")){
                poolGroupSS += groupSS.split("of")(0).replaceAll("\\D+","")+":"+groupSS.split("of")(1).replaceAll("\\D+","")
              } else {
                poolGroupSS += groupSS.replaceAll("\\D+","")
              }
            })

            /*================== HEURISTICS ==================*/
            /*================== LET'S GO.. ==================*/

            /*L2-Pool: Filter Matches of the form 'X/Y'*/
            for(groupSS <- poolGroupSS){
              if(groupSS.contains(",") && groupSS.split(",").length == 2){
                val potentialSS = groupSS.split(",")(1).toInt // E.g 155/510 -> 510. Check for other subgroups (LHS) summing up to 510
                val subgroupSS = {
                  poolGroupSS.filter(ss => ss.contains(",") && ss.split(",")(1).equalsIgnoreCase(potentialSS.toString))
                }.toList.map(_.split(",")(0).toInt)
                if(subgroupSS.sum == potentialSS && !poolGroupSS.find(_.equalsIgnoreCase(potentialSS.toString)).isDefined && subgroupSS.length > 1){
                  poolGroupSS += potentialSS.toString
                }
                if(poolGroupSS.filter(ss => ss.contains(",")).map(ss => ss.split(",")(1)).toList.count(_.equalsIgnoreCase(potentialSS.toString)) >=2
                  && !poolGroupSS.find(_.equalsIgnoreCase(potentialSS.toString)).isDefined){
                  poolGroupSS += potentialSS.toString
                }
              }
//              else if(groupSS.contains(":") && groupSS.split(":").length > 1){
//                val potentialSS = groupSS.split(":")(1).toInt // E.g 155/510 -> 510. Check for other subgroups (LHS) summing up to 510
//                val subgroupSS = {
//                    poolGroupSS.filter(ss => ss.contains(":") && ss.split(":")(1).equalsIgnoreCase(potentialSS.toString))
//                  }.toList.map(_.split(":")(0).toInt)
//                if(subgroupSS.sum == potentialSS){
//                  L2_SS += potentialSS
//                }
//              }
            }
            val poolGroupSSVal = poolGroupSS.filterNot(ss => ss.contains(",") || ss.contains(":")).map(_.toInt)

            /*Apply Calculations to the pools of OL1, NL1, NL1Neg and L2 depending on the empty-ness of the single pools*/
            if(!(poolInitialSS.isEmpty || poolActualSS.isEmpty || poolActualSSNegative.isEmpty || poolGroupSSVal.isEmpty)){
              for(initialSS <- poolInitialSS){
                for(actNegSS <- poolActualSSNegative){
                  if(poolActualSS.contains(initialSS-actNegSS)){
                    val potentialAct = initialSS-actNegSS
                    val potentialSubSet = findSubsetSum(poolGroupSSVal.toList,potentialAct)
                    if(potentialSubSet != null && potentialSubSet.size > 1){
                      OL1_SS += initialSS
                      NL1_SS += potentialAct
                      L2_SS ++= potentialSubSet.toList
                    }
//                    if(!(poolGroupSSVal.sum < potentialAct || poolGroupSSVal.contains(potentialAct))){
////                      val potentialSubsets = poolGroupSSVal.sorted(Ordering[Int].reverse).sliding(2) // subset of L2's which count to potentialAct
////                      for(subset <- potentialSubsets){
////                        if(subset.sum == potentialAct){
////                          OL1_SS += initialSS
////                          NL1_SS += potentialAct
////                          L2_SS ++= subset.toList
////                        }
////                      }
//                    }
                    if(OL1_SS.isEmpty || NL1_SS.isEmpty || L2_SS.isEmpty){
                      OL1_SS += initialSS
                      NL1_SS += potentialAct
                    }
                  }
                }
                if(OL1_SS.isEmpty && poolUsualKC.filterNot(poolActualSSNegative.contains(_)).contains(initialSS)){
                  OL1_SS += initialSS
                }
              }
            } else if(!(poolActualSS.isEmpty || poolActualSSNegative.isEmpty || poolGroupSSVal.isEmpty)){
              for(actualSS <- poolActualSSNegative){
                val potentialSubSet = findSubsetSum(poolGroupSSVal.toList,actualSS)
                if(potentialSubSet != null && potentialSubSet.size > 1){
                  NL1_SS += actualSS
                  L2_SS ++= potentialSubSet.toList
                }
              }
            } else if(!(poolInitialSS.isEmpty || poolActualSSNegative.isEmpty || poolGroupSSVal.isEmpty)){

            } else if(!(poolInitialSS.isEmpty || poolActualSS.isEmpty || poolGroupSSVal.isEmpty)){

            } else if(!(poolInitialSS.isEmpty || poolActualSS.isEmpty || poolActualSSNegative.isEmpty)){

            } else if(!(poolActualSSNegative.isEmpty || poolGroupSSVal.isEmpty)){

            } else if(!(poolInitialSS.isEmpty || poolActualSS.isEmpty)){

            } else if(!(poolInitialSS.isEmpty || poolGroupSSVal.isEmpty)){

            } else if(!(poolActualSS.isEmpty || poolActualSSNegative.isEmpty)){

            } else if(!(poolGroupSSVal.isEmpty)){

            } else if(!(poolActualSS.isEmpty)){

            } else if(!(poolActualSS.isEmpty || poolGroupSSVal.isEmpty)){

            } else if(poolInitialSS.isEmpty && poolActualSS.isEmpty && poolActualSSNegative.isEmpty && poolGroupSSVal.isEmpty){

            }

            /*Heuristic: SlidingWindow Approach on KC-Matches*/


            val innerFinalMap = mutable.Map.empty[String,List[Int]]
            innerFinalMap += "initialSampleSize" -> OL1_SS.toList
            innerFinalMap += "actualSampleSize" -> NL1_SS.toList
            innerFinalMap += "groupSampleSizes" -> L2_SS.toList
//            val outerFinalMap = mutable.Map.empty[String,mutable.Map[String,List[Int]]]
//            outerFinalMap += FilenameUtils.getBaseName(fileString) -> innerFinalMap

            finalEvaluationList += FilenameUtils.getBaseName(fileString) -> innerFinalMap
//            matchesInitialSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "InitialSS" + " ==> " + ss))
//            poolInitialSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolInitialSS" + " ==> " + ss))
//            matchesActualSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "ActualSS" + " ==> " + ss))
//            poolActualSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolActualSS" + " ==> " + ss))
//            matchesActualSSNegative.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "ActualSSNegative" + " ==> " + ss))
//            poolActualSSNegative.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolActualSSNegative" + " ==> " + ss))
//            matchesGroupSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "GroupSS" + " ==> " + ss))
//            poolGroupSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolGroupSS" + " ==> " + ss))
//            matchesUsualKC.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "UsualKC" + " ==> " + ss))
//            info("SS_POS_Map: FILE: " + FilenameUtils.getBaseName(fileString))
//            for(i <- 0 to SS_Pos_Map.size-1){
//              info("Match: " + SS_Pos_Map(i) + " ==> "+i)
//            }
            OL1_SS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "OL1_SS" + " ==> " + ss))
            NL1_SS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "NL1_ss" + " ==> " + ss))
            L2_SS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "L2_SS" + " ==> " + ss))
          }
        }
      }
    }
    /*EVALUATION ON GROUND TRUTH*/
    info("===== EVALUATION ON GROUND TRUTH =====")
    /*OL1 Sample Sizes*/
    info("===== Prec/Rec OL1 =====")
    val heuristic_ol1 = mutable.ListBuffer.empty[Int]
    for(key <- finalEvaluationList.keys){
      if(!finalEvaluationList.get(key).get("initialSampleSize").isEmpty){
        heuristic_ol1 += finalEvaluationList.get(key).get("initialSampleSize").mkString.toInt
      }
    }
    val precision_ol1 = (heuristic_ol1.intersect(gt_ol1).length)/(heuristic_ol1.length).toFloat
    info("Precision: " + precision_ol1)
    val recall_ol1 = (heuristic_ol1.intersect(gt_ol1).length)/(gt_ol1.length).toFloat
    info("Recall: " + recall_ol1)


    info("===== Prec/Rec NL1 =====")
    val heuristic_nl1 = mutable.ListBuffer.empty[Int]
    for(key <- finalEvaluationList.keys){
      if(!finalEvaluationList.get(key).get("actualSampleSize").isEmpty){
        heuristic_nl1 += finalEvaluationList.get(key).get("actualSampleSize").mkString.toInt
      }
    }
    val precision_nl1 = (heuristic_nl1.intersect(gt_nl1).length)/(heuristic_nl1.length).toFloat
    info("Precision: " + precision_nl1)
    val recall_nl1 = (heuristic_nl1.intersect(gt_nl1).length)/(gt_nl1.length).toFloat
    info("Recall: " + recall_nl1)

    info("===== Prec/Rec L2 - Exact Matches =====")
    val heuristic_l2 = mutable.ListBuffer.empty[List[Int]]
    for(key <- finalEvaluationList.keys){
      if(!finalEvaluationList.get(key).get("actualSampleSize").isEmpty){
        heuristic_l2 += finalEvaluationList.get(key).get("actualSampleSize")
      }
    }
    val precision_l2_exact = (heuristic_l2.intersect(gt_l2).length)/(heuristic_l2.length).toFloat
    info("Precision: " + precision_l2_exact)
    val recall_l2_exact = (heuristic_l2.intersect(gt_l2).length)/(gt_l2.length).toFloat
    info("Recall: " + recall_l2_exact)
    info("===== Prec/Rec L2 - Partial Matches =====")
    var partial_matches = 0
    val for_loop = new Breaks

    for(found_l2 <- heuristic_l2){
      for_loop.breakable{
        for(rec_l2 <- gt_l2){
          if(found_l2.intersect(rec_l2).nonEmpty){
            partial_matches += 1
            for_loop.break()
          }
        }
      }
    }
    val precision_l2_partial = (partial_matches)/(heuristic_l2.length).toFloat
    info("Precision: " + precision_l2_partial)
    val recall_l2_partial = (partial_matches)/(gt_l2.length).toFloat
    info("Recall: " + recall_l2_partial)

    info("FilesCount: " + filesCount)
//    val string = "145 people were excluded"
//    for(regex <- patternsGroupSampleSize){
//      val matches = regex.findAllIn(string).matchData
//      while (matches.hasNext){
//        val currentMatch = matches.next()
//        info("current match: " + currentMatch.toString() + " ===> " + regex)
//      }
//    }
  }

  test("Other Regex Testing"){
//    val testOther = new Regex("\\d+\\s*(\\(\\d+\\s*%\\))?\\s*received")
    val testOther = new Regex("\\d+([,\\s*]\\d{3})*\\s*(\\(\\s*\\d+([,.]\\d+)?\\s*%\\))?\\s*(people|patients|participants|subjects|cases|persons)?\\s*received")
//    val testOther = new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,20}\\s*were\\s*included\\s*\\D{0,20}\\s*analysis")
//    val testOther = new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|persons)?\\s*in\\s*the\\s*group")
    val string = "43 persons received"

    val matches = testOther.findAllIn(string)
    while (matches.hasNext){
      val currentMatch = matches.next()
      info("Match: " + currentMatch)
    }
  }

  def findSubsetSum(numbers: List[Int], sum: Int):ArrayBuffer[Int] = {
    val subset = mutable.ArrayBuffer.empty[Int]
    for (i <- 0 until numbers.length){
      if (findSubsetSum(numbers, i, subset, sum)) {
        return subset
      }
    }
    null
  }

  def findSubsetSum(numbers: List[Int], index: Int, subset: ArrayBuffer[Int], sum: Int):Boolean = {
    if (index >= numbers.length) {
      return false
    }

    if (sum - numbers(index) == 0) {
      subset += numbers(index)
      return true
    }

    if (sum - numbers(index) < 0) {
      return false
    }

    for ( i <- index+1 until numbers.length) {
      if (findSubsetSum(numbers, i, subset, sum - numbers(index))) {
        subset.add(numbers(index))
        return true
      }
    }
    false
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      info("Dir does not exist")
      List[File]()
    }
  }

  def listFilesAndFilesSubDirectories(dir: String):List[File] = {
    val directory = new File(dir)
    val fList = directory.listFiles()
    val fListToReturn = mutable.ListBuffer.empty[File]
    for(file <- fList){
      if(file.isFile){
        fListToReturn += file
      } else if(file.isDirectory){
        listFilesAndFilesSubDirectories(file.getAbsolutePath)
      }
    }
    fListToReturn.toList
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def deleteFile(filename: String) = { new File(filename).delete() }

  def convertPDFtoText(path: String): List[String] = {
    val paperLink = path
    val text = new PDFTextExtractor(paperLink).pages
//    if (!new File(paperLink + ".text").exists()) {
//      val pw = new PrintWriter(new File(paperLink + ".txt"))
//      pw.write(text.map(_.toLowerCase()).mkString("\n\n"))
//      pw.close()
//    }
    text
  }
}
