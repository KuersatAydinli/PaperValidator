package SampleSizeExtractorTestPackage

import java.io._
import java.util

import au.com.bytecode.opencsv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import com.google.common.base.CharMatcher
import helper.pdfpreprocessing.pdf.entity.{Table => trapRangeTable}
import helper.pdfpreprocessing.pdf.{PDFLoader, PDFTableExtractor, PDFTextExtractor}
import helper.statcheck.{Statchecker => StatChecker}
import org.apache.commons.io.FilenameUtils
import org.apache.pdfbox.pdmodel.PDDocument
//import org.python.core.PyInteger
//import org.python.util.PythonInterpreter
import org.scalatest.FunSuite

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.control.Breaks
import scala.util.matching.Regex

/**
  * Created by Aydinli on 26.01.2017.
  */
class SampleSizeExtractorTest extends FunSuite{

  test("GetSampleSize"){
    val paperDir = "test/TestPDFs/manuscript4.pdf"
    val pdfDoc = PDDocument.load(new File(paperDir))
    val pdfText = convertPDFtoText(paperDir)
    val statChecker = StatChecker

    assert(pdfText.length == 38)
    val sampleSize = statChecker.extractSampleSize(pdfText)

//    if (new File("test/TestPDFs/sampleSizeString" + ".text").exists()){
//      deleteFile("test/TestPDFs/sampleSizeString" + ".txt")
//    }
//
//    if (!new File("test/TestPDFs/sampleSizeString" + ".text").exists()) {
//      val pw = new PrintWriter(new File("test/TestPDFs/sampleSizeString" + ".txt"))
//      pw.write(sampleSize)
//      pw.close()
//    }

    info("Sample Size: " + sampleSize)
    assert(sampleSize.length > 1)
  }

  test("Get SampleSize Context"){
    val paperDir = "test/TestPDFs/2004_12404.pdf"
    val pdfDoc = PDDocument.load(new File(paperDir))
    val pdfText = convertPDFtoText(paperDir)
    val statChecker = StatChecker
    val relativeSupport = statChecker.getRelativeGroupSupport

    val sampleSizeContext = statChecker.extractSampleSizeContext(pdfText)
    info("PDF Name: " + paperDir)
    info("ContextMapSize : " + sampleSizeContext.size)
    sampleSizeContext foreach((entry) => info(entry._1 + " ===> " + "Support: " +
      relativeSupport(entry._1.getOrElse(entry._1.head._1,0)) + " ===> " + entry._2))
    info("========================================================================================================================")
    info("========================================================================================================================")
//    val filteredContext = statChecker.filterSampleSizeContext(sampleSizeContext)
//    info("Size Filtered Context: " + filteredContext.size)
//    filteredContext foreach((entry) => info(entry._1 + " ===> " + entry._2))
  }

//  test("Test new Map Structure"){
//    info("HEy yyyyy")
//    val paperDir = "test/TestPDFs/2004_12404.pdf"
//    val pdfDoc = PDDocument.load(new File(paperDir))
////    val pdfText = convertPDFtoText(paperDir)
//    val statChecker = StatChecker
//
//    val patternMap = statChecker.extractSampleSizeFromPaper(pdfText)
//    info("patternMapSize : " + patternMap.size)
//    for(pattern <- patternMap){
//      info("Pattern: " + pattern._1.toString + " ===> Support: " + pattern._2.support)
//    }
////  }

  test("Kuersat_Classifier"){
    info("Test Regex Precision...")
    val PdfPath = "test/TestPDFs"

    val testListRegex = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}men"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}children"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}residents"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}students"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}respondents"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}adults"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}procedures"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}volunteers"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}employees"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}users"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}individuals"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}managers"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}firms"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}establishments"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("sample\\s*of\\s*\\D{0,20}\\d+([,\\s*]\\d{3})*"),
//      new Regex("sample\\D{0,10}included\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s?cohort\\D{0,20}of\\D{0,15}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,25}recruited"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("enrolled\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,20}\\d+([,\\s*]\\d{3})*"))
    val testListRegexNonOverfitted = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}men"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("\\s*data\\D{0,10}of\\D{0,5}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,10}from\\D{0,5}\\d+([,\\s*]\\d{3})*"))
    val patternMatchesInGT = mutable.Map.empty[Regex, Int] // #Matches per Pattern in the Ground Truth
    val bufferedSource = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")

//    testListRegexNonOverfitted.par.foreach(r => patternMatchesInGT(r) = 0)
    for(regex <- testListRegexNonOverfitted){
      patternMatchesInGT(regex) = 0
    }

    for(line <- bufferedSource.getLines()){
      val cols = line.split(",").map(_.trim)
//      testListRegexNonOverfitted.par.foreach(r =>
//        if(r.findAllIn(cols(5)).matchData.nonEmpty){
//          patternMatchesInGT.update(r,patternMatchesInGT(r)+1)
//        })
      for(regex <- testListRegexNonOverfitted){
        if(regex.findAllIn(cols(5)).matchData.nonEmpty){
          patternMatchesInGT.update(regex,patternMatchesInGT(regex)+1)
        }
      }
    }
    bufferedSource.close

    val files = getListOfFiles(PdfPath)
    val patternMatchesTotal = mutable.Map.empty[Regex, Int]
    val patternMatchesFiltered = mutable.Map.empty[Regex, Int]
//    testListRegexNonOverfitted.par.foreach(r => patternMatchesTotal(r) = 0)
//    testListRegexNonOverfitted.par.foreach(r => patternMatchesFiltered(r) = 0)
    for(regex <- testListRegexNonOverfitted){
      patternMatchesTotal(regex) = 0
      patternMatchesFiltered(regex) = 0
    }
    val matchesKuersatClassifier = new ListBuffer[String]()
//    val KuersatClassifierMap = mutable.Map.empty[String,String]

    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
    val writer = new CSVWriter(new FileWriter("test/PDFLib/KuersatClassifier_Matches_new.csv"))
    val csv_writer = new CSVWriter(new FileWriter("test/PDFLib/KuersatClassifier_full.csv"))
//    val CsvHeader = List[String]("PDF_Name","Match")
//    writer.writeRow(CsvHeader)
    val CsvHeader = List[String]("PDF_Name","Match","T-Test Permutation","Distance")
    csv_writer.writeRow(CsvHeader)

    val methodSource = Source.fromFile("statterms/templates/followup/methods.csv")
    val testPermutations : ArrayBuffer[String] = new ArrayBuffer[String]()
    for (line <- methodSource.getLines()){
      if(line.split(";")(0).contains("test")){
        testPermutations += line.split(";")(0)
        line.split(";")(1).split(",").foreach(perm => {
//          testPermutations += perm.replaceAll("\\s+","").toLowerCase()
          testPermutations += perm
        })
      }
    }

    for (file <- files){
      val matchesInFile = new ListBuffer[String]()
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        val pdfDoc = PDDocument.load(new File(fileString))
        val pdfText = convertPDFtoText(fileString)
//        testListRegexNonOverfitted.par.foreach(regex =>
//          if(regex.findAllIn(pdfText.mkString).nonEmpty){
//            patternMatchesTotal.update(regex,patternMatchesTotal(regex)+regex.findAllIn(pdfText.mkString).length)
//            patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)+regex.findAllIn(pdfText.mkString).length)
//            val totalMatches = regex.findAllIn(pdfText.mkString).matchData
//            while(totalMatches.hasNext){
//              val currentMatch = totalMatches.next().toString()
//              matchesKuersatClassifier += currentMatch
//              if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
//                "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
//                "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*")){
//                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
//                matchesKuersatClassifier -= currentMatch
//              } else if(currentMatch.contains("%")){
//                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
//                matchesKuersatClassifier -= currentMatch
//              } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
//                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
//                matchesKuersatClassifier -= currentMatch
//              } else if(currentMatch.matches("\\d+[/)\\]]\\D*")){
//                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
//                matchesKuersatClassifier -= currentMatch
//              }
//            }
//        })
        for(regex <- testListRegexNonOverfitted){
          if(regex.findAllIn(pdfText.mkString).nonEmpty){
            patternMatchesTotal.update(regex,patternMatchesTotal(regex)+regex.findAllIn(pdfText.mkString).length)
            patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)+regex.findAllIn(pdfText.mkString).length)
            val totalMatches = regex.findAllIn(pdfText.mkString).matchData
            while(totalMatches.hasNext){
              val currentMatch = totalMatches.next().toString()
              matchesKuersatClassifier += currentMatch
              matchesInFile += currentMatch
              if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.contains("%")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(bracketList.exists(currentMatch.contains(_))){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[-]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[,]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
                "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
                "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
                "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              }
            }
          }
        }
        var testPositionMap = mutable.Map.empty[String,Int]
        var containsTest = false
        for(perm <- testPermutations){
          if(pdfText.mkString.contains(" " + perm + " ")){
            containsTest = true
            val position = pdfText.mkString.indexOf(" " + perm + " ")
            testPositionMap += " "+perm+" " -> position
          }
        }
        for(matches <- matchesInFile.distinct){
          if(!containsTest){
            val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "),"null",
            "-1")
            csv_writer.writeRow(csvEntry)
          } else {
            var distances = mutable.ListBuffer.empty[Int]
            testPositionMap.values.par.foreach(pos => {
              val currentDistance = Math.abs(pdfText.mkString.indexOf(matches) - pos)
              distances += currentDistance
            })
            val minDistance = distances.min
            var minDistancePermutation = "foo"
            for(entry <- testPositionMap){
              if(Math.abs(pdfText.mkString.indexOf(matches) - entry._2) == minDistance){
                minDistancePermutation = entry._1
              }
            }
            //        val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "))
            val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "),
              minDistancePermutation,minDistance.toString)
            csv_writer.writeRow(csvEntry)
            //        writer.writeRow(csvEntry)
          }
        }
      }
    }
    csv_writer.close()
//    writer.close()



//    info("Pattern Matches in GT")
//    for(entry <- patternMatchesInGT){
//      info("%-60s ==> %s".format(entry._1.toString(),entry._2).toString)
//    }
//    info("===============================================================================")
//    info("Pattern Matches Total")
//    for(entry <- patternMatchesTotal){
//      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternMatchesFiltered(entry._1)).toString)
//    }
//
//    val patternPrecision = mutable.Map.empty[Regex,Float]
//    val patternPrecisionFiltered = mutable.Map.empty[Regex,Float]
//    for(regex <- testListRegexNonOverfitted){
//      patternPrecision(regex) = patternMatchesInGT(regex).toFloat / patternMatchesTotal(regex)
//      patternPrecisionFiltered(regex) = patternMatchesInGT(regex).toFloat / patternMatchesFiltered(regex)
//    }
//
//    info("===============================================================================")
//    info("Pattern Precision")
//
////    val pw = new PrintWriter(new File("test/PDFLib/Pattern_Precision_Filtering.txt"))
//    for(entry <- patternPrecision){
//      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
////      pw.write("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
////      pw.write("\n")
//    }
////    pw.close()
//    info("KuersatClassifier # Matches: " + matchesKuersatClassifier.length)
////    matchesKuersatClassifier.par.foreach(m => info("Match: " + m))
//    info("KuersatClassifier distincst: " + matchesKuersatClassifier.distinct.length)
//    for(matches <- matchesKuersatClassifier.distinct){
//      info("Current Match: " + matches.replaceAll("\\n|\\r"," "))
//    }
//
//    info("Recall Calculation")
//    val paperMatch = mutable.Map.empty[String,Boolean]
//    for(file <- files){
//      val fileString = file.toString
//      if(FilenameUtils.getExtension(fileString).equals("txt") && FilenameUtils.getName(fileString) != "sampleSizesExtracted"){
//        paperMatch(FilenameUtils.getName(fileString).replace(".pdf.txt",".txt")) = false
//      }
//    }
//
//    val foundMatchesInGT = new ListBuffer[String]()
//    val bufferedSourceSecond = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")
//    for(line <- bufferedSourceSecond.getLines()){
//      val cols = line.split(",").map(_.trim)
//      for(matches <- matchesKuersatClassifier.distinct){
//        if(cols(5).toLowerCase.replaceAll("\\s","").contains(matches.toLowerCase.replaceAll("\\s",""))){
//          info("Found: " + cols(5))
//          paperMatch(cols(0)) = true
//          foundMatchesInGT += cols(5)
//        }
//      }
//    }
//    bufferedSourceSecond.close
//    var papermatch = 0
//    for(papermatches <- paperMatch){
//      if(papermatches._2){
//        papermatch += 1
//        info("Matched Paper: " + papermatches._1)
//      }
//    }
//    info("TotalFound: " + foundMatchesInGT.distinct.length)
//    info("TotalPaperMatches: " + papermatch)
  }

  test("Kuersat_Classifier_Full_Corpus"){
    info("Test Kuersat Classifier Full Corpus...")
    val PdfPath = "test/TestPDFs"

    val JournalPath = "F:\\Dropbox\\Dropbox\\all papers"

    val testListRegex = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}men"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}children"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}residents"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}students"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}respondents"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}adults"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}procedures"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}volunteers"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}employees"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}users"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}individuals"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}managers"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}firms"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}establishments"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("sample\\s*of\\s*\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      //      new Regex("sample\\D{0,10}included\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s?cohort\\D{0,20}of\\D{0,15}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,25}recruited"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("enrolled\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,20}\\d+([,\\s*]\\d{3})*"))
    val testListRegexNonOverfitted = mutable.MutableList(
      new Regex("\\b(study|sample)\\b\\D{0,15}\\d+\\D{0,15}"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*were\\s*assigned\\s*to"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}individuals"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}adults"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,25}recruited"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("\\b(\\D{0,15})\\b(enrolled)\\s*\\d+([,\\s*]\\d{3})*\\D{0,15}"),
      new Regex("\\s*data\\D{0,10}of\\D{0,5}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,10}from\\D{0,5}\\d+([,\\s*]\\d{3})*"))

    val patternMatchesInGT = mutable.Map.empty[Regex, Int] // #Matches per Pattern in the Ground Truth
    val bufferedSource = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")
    val listMappedSS = mutable.ListBuffer.empty[String] // list of all SS which are mapped to a t-test in the ground truth
    val listMappedSS_exact = mutable.ListBuffer.empty[String] //list of all SS which are exactly matched to a ttest
    val listTTestPapers = mutable.ListBuffer.empty[String]

    val bufferedSource_mapped_ss = Source.fromFile("test/PDFLib/KC_Task1_one_SS_per_t-test.csv")
    val GT_correct_SS = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes_copy.csv")
    for(line <- bufferedSource_mapped_ss.getLines()){
      val cols = line.split(",").map(_.trim)
      val mapped_ss = cols(3)
      listMappedSS += mapped_ss
    }
    bufferedSource_mapped_ss.close()

    val pdf_correctSS_tuples = mutable.ListBuffer.empty[(String, Int)]
    for(line <- GT_correct_SS.getLines()){
      val cols = line.split(",").map(_.trim)
      if(cols(6).equals("true")){
        val pdfName = cols(0).replace(".txt","")
        val sampleSize = cols(4).toInt
        pdf_correctSS_tuples += (pdfName -> sampleSize)

        val mapped_ss = cols(5)
        listMappedSS_exact += mapped_ss
        if(!listTTestPapers.contains(cols(0))){
          listTTestPapers += pdfName
        }
      }
    }
    val pdf_correctSS_distinct = pdf_correctSS_tuples.distinct
    GT_correct_SS.close()

    val correctSS_list = listMappedSS_exact.par.map(ss => ss.replaceAll("\\D+","").toInt).distinct
    val foundSS_list = mutable.ListBuffer.empty[Int]

    for(regex <- testListRegexNonOverfitted){
      patternMatchesInGT(regex) = 0
    }

    for(line <- bufferedSource.getLines()){
      val cols = line.split(",").map(_.trim)
      //      testListRegexNonOverfitted.par.foreach(r =>
      //        if(r.findAllIn(cols(5)).matchData.nonEmpty){
      //          patternMatchesInGT.update(r,patternMatchesInGT(r)+1)
      //        })
      for(regex <- testListRegexNonOverfitted){
        if(regex.findAllIn(cols(5)).matchData.nonEmpty){
          patternMatchesInGT.update(regex,patternMatchesInGT(regex)+1)
        }
      }
    }
    bufferedSource.close

    val files = getListOfFiles(PdfPath)
    val baseDirectory = new File(JournalPath)
    val allFiles = recursiveListFiles(baseDirectory)
    val patternMatchesTotal = mutable.Map.empty[Regex, Int]
    val patternMatchesFiltered = mutable.Map.empty[Regex, Int]
    //    testListRegexNonOverfitted.par.foreach(r => patternMatchesTotal(r) = 0)
    //    testListRegexNonOverfitted.par.foreach(r => patternMatchesFiltered(r) = 0)
    for(regex <- testListRegexNonOverfitted){
      patternMatchesTotal(regex) = 0
      patternMatchesFiltered(regex) = 0
    }
    val matchesKuersatClassifier = new ListBuffer[String]()
    //    val KuersatClassifierMap = mutable.Map.empty[String,String]

    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
//    val csv_writer = new CSVWriter(new FileWriter("test/PDFLib/KuersatClassifier_corpus_extended.csv"))
//    val CsvHeader = List[String]("PDF_Name","Match","Sample Size","Distance T-Test")
//    csv_writer.writeRow(CsvHeader)
//    val writer = new CSVWriter(new FileWriter("test/PDFLib/KuersatClassifier_Matches_new.csv"))
//    val csv_writer = new CSVWriter(new FileWriter("test/PDFLib/KuersatClassifier_full.csv"))
//    //    val CsvHeader = List[String]("PDF_Name","Match")
//    //    writer.writeRow(CsvHeader)
//    val CsvHeader = List[String]("PDF_Name","Match","T-Test Permutation","Distance")
//    csv_writer.writeRow(CsvHeader)
    val methodSource = Source.fromFile("statterms/templates/followup/methods.csv")
    val testPermutations : ArrayBuffer[String] = new ArrayBuffer[String]()
    for (line <- methodSource.getLines()){
      if(line.split(";")(0).contains("test")){
        testPermutations += line.split(";")(0)
        line.split(";")(1).split(",").foreach(perm => {
          testPermutations += perm
        })
      }
    }

    val papersWithRightMatch = mutable.Map.empty[String,Boolean] // Map <<t-test paper - Bool right sample size found>>
    for(paper <- listTTestPapers.distinct){
      papersWithRightMatch += paper -> false
    }
    val papersMatchesMap = mutable.Map.empty[String,List[String]] // Map <<Paper - Matches in Paper from KC>>
    for (file <- files){
      val matchesInFile = new ListBuffer[String]()
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf") &&
        listTTestPapers.distinct.contains(FilenameUtils.getBaseName(fileString))){
        val pdfText = convertPDFtoText(fileString)
        //        testListRegexNonOverfitted.par.foreach(regex =>
        //          if(regex.findAllIn(pdfText.mkString).nonEmpty){
        //            patternMatchesTotal.update(regex,patternMatchesTotal(regex)+regex.findAllIn(pdfText.mkString).length)
        //            patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)+regex.findAllIn(pdfText.mkString).length)
        //            val totalMatches = regex.findAllIn(pdfText.mkString).matchData
        //            while(totalMatches.hasNext){
        //              val currentMatch = totalMatches.next().toString()
        //              matchesKuersatClassifier += currentMatch
        //              if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
        //                "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
        //                "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*")){
        //                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
        //                matchesKuersatClassifier -= currentMatch
        //              } else if(currentMatch.contains("%")){
        //                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
        //                matchesKuersatClassifier -= currentMatch
        //              } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
        //                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
        //                matchesKuersatClassifier -= currentMatch
        //              } else if(currentMatch.matches("\\d+[/)\\]]\\D*")){
        //                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
        //                matchesKuersatClassifier -= currentMatch
        //              }
        //            }
        //        })
        for(regex <- testListRegexNonOverfitted){
          if(regex.findAllIn(pdfText.mkString).nonEmpty){
            patternMatchesTotal.update(regex,patternMatchesTotal(regex)+regex.findAllIn(pdfText.mkString).length)
            patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)+regex.findAllIn(pdfText.mkString).length)
            val totalMatches = regex.findAllIn(pdfText.mkString).matchData
            while(totalMatches.hasNext){
              val currentMatch = totalMatches.next().toString()
              matchesKuersatClassifier += currentMatch
              matchesInFile += currentMatch
              if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.contains("%")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(bracketList.exists(currentMatch.contains(_))){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[-]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[,]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
                "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
                "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
                "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*\\s+[Hh]ow\\s+\\D*|\\d\\D*s+[Aa]lthough\\D*|\\d+\\D*\\s+[Tt]he\\s+\\D*" +
                "|\\d+\\D*\\s+[Mm]any\\s+\\D*|\\d+\\D*\\s+[Oo]ften\\s+\\D*|\\d+\\D*\\s+found\\s+\\D*|\\d+\\D*\\s+[Oo]n\\s+\\D*" +
                "|\\d+\\D*[Aa]mong\\D*|\\d+\\D*[Ss]how\\D*|\\d+\\D*[Tt]han\\D*|\\d+\\D*[Pp]ercent\\D*|\\d+\\D*[Ww]hich\\D*" +
                "|\\d+\\D*[Dd]uring\\D*|\\d+\\D*as\\s*well\\s*as\\D*|\\d+\\D*[Aa]nother\\D*|\\d+\\D*[Cc]haracteristics\\D*" +
                "|\\d+\\D*[Aa]ttribute\\D*|\\d+\\D*[Mm]ost\\D*|\\d+\\D*\\s+[Bb]ut\\s+\\D*|\\d+\\D*[Aa]bout\\D*" +
                "|\\d+\\D*[Ww]hether\\D*|\\d+\\D*Number\\D*") &&
                !currentMatch.matches("\\d+\\D*\\s+[Oo]f\\s*the\\s+\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(!CharMatcher.ASCII.matchesAllOf(currentMatch)){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              }
            }
          }
        }
        //        var testPositionMap = mutable.Map.empty[String,Int]
        //        var containsTest = false
        //        for(perm <- testPermutations){
        //          if(pdfText.mkString.contains(" " + perm + " ")){
        //            containsTest = true
        //            val position = pdfText.mkString.indexOf(" " + perm + " ")
        //            testPositionMap += " "+perm+" " -> position
        //          }
        //        }
        //        for(matches <- matchesInFile.distinct){
        //          if(!containsTest){
        //            val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "),
        //              matches.replaceAll("\\n|\\r"," ").replaceAll("\\D+",""),"-1")
        //            csv_writer.writeRow(csvEntry)
        //          } else {
        //            var distances = mutable.ListBuffer.empty[Int]
        //            testPositionMap.values.par.foreach(pos => {
        //              val currentDistance = Math.abs(pdfText.mkString.indexOf(matches) - pos)
        //              distances += currentDistance
        //            })
        //            val minDistance = distances.min
        //            var minDistancePermutation = "foo"
        //            for(entry <- testPositionMap){
        //              if(Math.abs(pdfText.mkString.indexOf(matches) - entry._2) == minDistance){
        //                minDistancePermutation = entry._1
        //              }
        //            }
        //            //        val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "))
        //            val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "),
        //              matches.replaceAll("\\n|\\r"," ").replaceAll("\\D+",""),minDistance.toString)
        //            csv_writer.writeRow(csvEntry)
        //            //        writer.writeRow(csvEntry)     .replaceAll("\\D+",""))
        //          }
        //        }
        val mostCommonSS = matchesInFile.map(ss => ss.replaceAll("\\D+","").toInt).distinct.toList.groupBy(identity).maxBy(_._2.length)._1
        foundSS_list += mostCommonSS
        for(matchInFile <- matchesInFile.map(ss => ss.replaceAll("\\D+","").toInt).distinct.toList){
          for(correctSS <- pdf_correctSS_distinct){
            if(FilenameUtils.getBaseName(fileString).equalsIgnoreCase(correctSS._1) && matchInFile == correctSS._2){
              papersWithRightMatch(FilenameUtils.getBaseName(fileString)) = true
            }
          }
        }
//        papersMatchesMap += FilenameUtils.getBaseName(fileString) -> matchesInFile.par.map(ss => ss.replaceAll("\\D+","").toInt).distinct.toList
        papersMatchesMap += FilenameUtils.getBaseName(fileString) -> matchesInFile.distinct.toList
      }
//      val matchesInFile_list = matchesInFile.par.map(ss => ss.replaceAll("\\D+","").toInt).distinct.toList
    }

    info("======================Precision/Recall======================")
    info("============================================================")
    val intersect_ss = correctSS_list.intersect(foundSS_list.distinct)
//    var counter_mapped_ss = 0
//    for(matchesKC <- matchesKuersatClassifier.distinct){
//      for(matches_mapped <- listMappedSS.distinct){
//        if(matchesKC.trim().equalsIgnoreCase(matches_mapped.trim())){
//          counter_mapped_ss += 1
//        }
//      }
//    }
//    info("Recall: " + counter_mapped_ss/listMappedSS.length.toFloat
    val precision_most_frequent = intersect_ss.length.toFloat/foundSS_list.length
    val correctSS_in_Matches = papersWithRightMatch.count(_._2.equals(true))
    val mappedSS_boolean = mutable.Map.empty[String,Boolean]
    val mappedSS_boolean_exact = mutable.Map.empty[String,Boolean]
    val KC_SS_boolean = mutable.Map.empty[String,Boolean]
    val KC_SS_boolean_exact = mutable.Map.empty[String,Boolean]

    for(matches_mapped <- listMappedSS){
      mappedSS_boolean += matches_mapped -> false
    }
    for(matches_mapped <- listMappedSS_exact){
      mappedSS_boolean_exact += matches_mapped -> false
    }

    for(matchesKC <- matchesKuersatClassifier){
      KC_SS_boolean += matchesKC -> false
      KC_SS_boolean_exact += matchesKC -> false
    }

    for(matchesKC <- matchesKuersatClassifier){
      for(matches_mapped <- listMappedSS){
        if(matchesKC.trim().equalsIgnoreCase(matches_mapped.trim())){
          mappedSS_boolean(matches_mapped) = true
          KC_SS_boolean(matchesKC) = true
        }
      }
    }

    for(matchesKC <- matchesKuersatClassifier){
      for(matches_mapped <- listMappedSS_exact){
        if(matchesKC.trim().equalsIgnoreCase(matches_mapped.trim())){
          mappedSS_boolean_exact(matches_mapped) = true
          KC_SS_boolean_exact(matchesKC) = true
        }
      }
    }


//    info("gemappte KC sample sizes: " + KC_SS_boolean_exact.values.count(_.equals(true))
//    info("Matches KC: " + matchesKuersatClassifier.distinct.length)
//    info("Precision: " + KC_SS_boolean.values.count(_.equals(true))/matchesKuersatClassifier.distinct.length.toFloat)
//    info("Precision exact: " + KC_SS_boolean_exact.values.count(_.equals(true))/matchesKuersatClassifier.distinct.length.toFloat)
//    info(mappedSS_boolean_exact.values.count(_.equals(true)).toString)
//    info(listMappedSS_exact.length.toFloat.toString)
//    info("Recall: " + mappedSS_boolean.values.count(_.equals(true))/listMappedSS.length.toFloat)
//    info("Recall exact: " + mappedSS_boolean_exact.values.count(_.equals(true)).toFloat/listMappedSS_exact.length)
//    csv_writer.close()
    //    writer.close()

//    info("Pattern Matches in GT")
//    for(entry <- patternMatchesInGT){
//      info("%-60s ==> %s".format(entry._1.toString(),entry._2).toString)
//    }
//    info("===============================================================================")
//    info("Pattern Matches Total")
//    for(entry <- patternMatchesTotal){
//      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternMatchesFiltered(entry._1)).toString)
//    }
//
//    val patternPrecision = mutable.Map.empty[Regex,Float]
//    val patternPrecisionFiltered = mutable.Map.empty[Regex,Float]
//    for(regex <- testListRegexNonOverfitted){
//      patternPrecision(regex) = patternMatchesInGT(regex).toFloat / patternMatchesTotal(regex)
//      patternPrecisionFiltered(regex) = patternMatchesInGT(regex).toFloat / patternMatchesFiltered(regex)
//    }
//
//    info("===============================================================================")
//    info("Pattern Precision")
//
////    val pw = new PrintWriter(new File("test/PDFLib/Pattern_Precision_Filtering.txt"))
//    for(entry <- patternPrecision){
//      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
////      pw.write("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
////      pw.write("\n")
//    }
////    pw.close()
//    info("KuersatClassifier # Matches: " + matchesKuersatClassifier.length)
////    matchesKuersatClassifier.par.foreach(m => info("Match: " + m))
//    info("KuersatClassifier distincst: " + matchesKuersatClassifier.distinct.length)
//    for(matches <- matchesKuersatClassifier.distinct){
//      info("Current Match: " + matches.replaceAll("\\n|\\r"," "))
//    }
//
//    info("Recall Calculation")
//    val paperMatch = mutable.Map.empty[String,Boolean]
//    for(file <- files){
//      val fileString = file.toString
//      if(FilenameUtils.getExtension(fileString).equals("txt") && FilenameUtils.getName(fileString) != "sampleSizesExtracted"){
//        paperMatch(FilenameUtils.getName(fileString).replace(".pdf.txt",".txt")) = false
//      }
//    }
//
//    val foundMatchesInGT = new ListBuffer[String]()
//    val bufferedSourceSecond = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")
//    for(line <- bufferedSourceSecond.getLines()){
//      val cols = line.split(",").map(_.trim)
//      for(matches <- matchesKuersatClassifier.distinct){
//        if(cols(5).toLowerCase.replaceAll("\\s","").contains(matches.toLowerCase.replaceAll("\\s",""))){
//          info("Found: " + cols(5))
//          paperMatch(cols(0)) = true
//          foundMatchesInGT += cols(5)
//        }
//      }
//    }
//    bufferedSourceSecond.close
//    var papermatch = 0
//    for(papermatches <- paperMatch){
//      if(papermatches._2){
//        papermatch += 1
//        info("Matched Paper: " + papermatches._1)
//      }
//    }
//    info("TotalFound: " + foundMatchesInGT.distinct.length)
//    info("TotalPaperMatches: " + papermatch)

  }

  test("Random t-test for rule building"){
    val PdfPath = "test/RandomTTestPapers"
    val testListRegexNonOverfitted = mutable.MutableList(
      new Regex("\\d+\\s*\\/\\s*\\d+"),
      new Regex("\\d+\\s*of\\s*\\d+"),
      new Regex("\\d+\\s*in\\s*the\\s*group"),
      new Regex("\\d+\\s*\\D{0,20}\\s*to\\s*\\D{0,40}\\s*and\\s*\\d+\\s*\\D{0,20}\\s*to"),
      new Regex("(study|sample)\\s+of\\D{0,15}\\d+\\D{0,15}"),
      new Regex("\\d+([,\\s*]\\d{3})*\\s*were\\s*assigned\\s*to"),
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
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("\\b(\\D{0,15})\\b(enrolled)\\s*\\d+([,\\s*]\\d{3})*\\D{0,15}"),
      new Regex("\\s*data\\D{0,10}of\\D{0,5}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,10}from\\D{0,5}\\d+([,\\s*]\\d{3})*"))

    val files = getListOfFiles(PdfPath)
    val matchesKuersatClassifier = new ListBuffer[String]()
    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
    val papersMatchesMap = mutable.Map.empty[String,List[String]] // Map <<Paper - Matches in Paper from KC>>
    for (file <- files){
      val matchesInFile = new ListBuffer[String]()
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        val pdfText = convertPDFtoText(fileString)
//        if (!new File("test/RandomTTestPapers/"+ FilenameUtils.getBaseName(fileString) + ".txt").exists()) {
//          val pw = new PrintWriter(new File("test/RandomTTestPapers/"+ FilenameUtils.getBaseName(fileString) + ".txt"))
//          pw.write(pdfText.map(_.toLowerCase()).mkString("\n\n"))
//          pw.close()
//        }
        for(regex <- testListRegexNonOverfitted){
          if(regex.findAllIn(pdfText.mkString).nonEmpty){
            val totalMatches = regex.findAllIn(pdfText.mkString).matchData
            while(totalMatches.hasNext){
              val currentMatch = totalMatches.next().toString()
              matchesKuersatClassifier += currentMatch
              matchesInFile += currentMatch
              if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*")){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.contains("%")){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(bracketList.exists(currentMatch.contains(_))){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[-]\\D*")){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[,]\\D*")){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
                "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
                "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
                "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*")){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*\\s+[Hh]ow\\s+\\D*|\\d\\D*s+[Aa]lthough\\D*|\\d+\\D*\\s+[Tt]he\\s+\\D*" +
                "|\\d+\\D*\\s+[Mm]any\\s+\\D*|\\d+\\D*\\s+[Oo]ften\\s+\\D*|\\d+\\D*\\s+found\\s+\\D*|\\d+\\D*\\s+[Oo]n\\s+\\D*" +
                "|\\d+\\D*[Aa]mong\\D*|\\d+\\D*[Ss]how\\D*|\\d+\\D*[Tt]han\\D*|\\d+\\D*[Pp]ercent\\D*|\\d+\\D*[Ww]hich\\D*" +
                "|\\d+\\D*[Dd]uring\\D*|\\d+\\D*as\\s*well\\s*as\\D*|\\d+\\D*[Aa]nother\\D*|\\d+\\D*[Cc]haracteristics\\D*" +
                "|\\d+\\D*[Aa]ttribute\\D*|\\d+\\D*[Mm]ost\\D*|\\d+\\D*\\s+[Bb]ut\\s+\\D*|\\d+\\D*[Aa]bout\\D*" +
                "|\\d+\\D*[Ww]hether\\D*|\\d+\\D*Number\\D*") &&
                !currentMatch.matches("\\d+\\D*\\s+[Oo]f\\s*the\\s+\\D*")){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(!CharMatcher.ASCII.matchesAllOf(currentMatch)){
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              }
            }
          }
        }
        papersMatchesMap += FilenameUtils.getBaseName(fileString) -> matchesInFile.distinct.toList
      }
    }
    papersMatchesMap.foreach(paper => paper._2.foreach(matches => info(paper._1 + " ====>> " + matches)))
  }

//  test("Test embedding Jython in Java/Scala"){
//    val pythonInterpreter = new PythonInterpreter()
//    pythonInterpreter.exec("import sys")
//    pythonInterpreter.exec("print sys")
//    pythonInterpreter.set("a", new PyInteger(42))
//    pythonInterpreter.exec("print a")
//    pythonInterpreter.exec("x = 2+2")
//    val x = pythonInterpreter.get("x")
//    info(x.toString)
//  }

  test("Test Table Extractor"){
    val TARGET_PDF = new File("test/RandomTTestPapers/bmj4_10_e005182.full.pdf")

    val tableRegex = new Regex("[Tt]able\\s*\\D+")
    val pagesIdx = mutable.ListBuffer.empty[Int] // Page Indices of pages containing a table
    val fileString = TARGET_PDF.toString
    val pdfText = convertPDFtoText(fileString)
    for(index <- 0 until pdfText.length){
      if(tableRegex.findAllIn(pdfText(index)).nonEmpty){
        pagesIdx += index
      }
    }
    val listTablesAll = mutable.ListBuffer.empty[trapRangeTable]

    for(index <- pagesIdx){
      val extractorCountLines = new PDFTableExtractor
      extractorCountLines.setSource(TARGET_PDF)
      extractorCountLines.addPage(index)
      val tables: util.List[trapRangeTable]= extractorCountLines.extract()
      val lineCount = tables.get(0).getRows.length

      var listTableColumnCount = mutable.Map.empty[trapRangeTable, Int]

      for(i <- 0 until lineCount){
        val pdfTableExtractor = new PDFTableExtractor
        pdfTableExtractor.setSource(TARGET_PDF)
        pdfTableExtractor.addPage(index)
        val exceptedLinesBefore = 0 until i toList
        val exceptedLinesAfter = i + 5 until lineCount toList
        val exceptedLinesTotal = List.concat(exceptedLinesBefore,exceptedLinesAfter)
        pdfTableExtractor.exceptLine(exceptedLinesTotal.toArray)
        val tables: util.List[trapRangeTable]= pdfTableExtractor.extract()
//        info("Window Index: " + i + " <======> Column Count " + tables(0).getColumnsCount)
//        tables.foreach(table => info(table.toHtml))
        tables.foreach(table => listTableColumnCount += table -> table.getColumnsCount)
      }
      val MapColumnCountTables = mutable.Map.empty[Int,List[trapRangeTable]]
      listTableColumnCount.values.toList.distinct.foreach(columnCount => {
        MapColumnCountTables += columnCount -> listTableColumnCount.filter(_._2 == columnCount).keySet.toList
      })

      var listFinalTables = mutable.ListBuffer.empty[trapRangeTable]
      MapColumnCountTables.values.foreach(tableList => {
        val tableMerged = PDFTableExtractor.mergeTablesAndFilter(tableList)
        listFinalTables += tableMerged
      })
      info("Page Inx: " + index)
      info("==== TABLES ====")
      listFinalTables.foreach(table => info(table.toHtml))
      listTablesAll ++= listFinalTables
    }
//    val textPositionExtractor = new TextPositionExtractor(pdDoc,3)
//    val textPositions: util.List[TextPosition] = textPositionExtractor.extract()
//    val listYPositions = mutable.ListBuffer.empty[Float]
//    textPositions.foreach(pos => listYPositions += pos.getY)
//    info("Distinct Y values: " + listYPositions.distinct.length)

////    textPositions.foreach(pos => info("TextPos: " + pos))
//    for(i <- 1 until 5){
//      info("TextPos: " + textPositions(i))
//      info("X: " + textPositions(i).getX)
//      info("X: " + textPositions(i).getY)
//      info("X: " + textPositions(i).getWidth)
//      info("X: " + textPositions(i).getHeight)
//    }

//    val listFinalTables = listTableColumnCount.filter(_._2 == listTableColumnCount.values.groupBy(identity).maxBy(_._2.size)._1).keySet.toList
    info("For Debugging")
//    val textStripper = new PDFTextStripper
//    textStripper.setStartPage(4)
//    textStripper.setEndPage(4)
//    val text = textStripper.getText(pdDoc)
//    info("TextStripper Text: " + text)
//    val linesCount = text.split("\n")
//    info("Lines Count: " + linesCount.length)
//    info("First Line" + text.split("\n")(0))

//    val exceptedLines = List[Int](0,1,2,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60)
//    val columnRanges = pdfTableExtractor.getColumnRanges(textPositions)
//    columnRanges.foreach(range => info("Range: " + range))

//    pdfTableExtractor.exceptLine(exceptedLines.toArray)
//    val tables: util.List[trapRangeTable]= pdfTableExtractor.extract()
//    tables.foreach(table => info("column count: " + table.getColumnsCount))
//    tables.foreach(table => {
//      table.getRows.foreach(row => info("Row String: " + row.toString))
//    })
//    info("============Cells==========")
//    tables.foreach(table => {
//      table.getRows.foreach(row => {
//        row.getCells.foreach(cell => info("Cell: " + cell.getContent))
//      })
//    })

//    info("To HTML")
//    tables.foreach(table =>{
//      info(table.toHtml)
//    })
  }

//  test("Test Tabula"){
//    val bae: BasicExtractionAlgorithm = new BasicExtractionAlgorithm()
//    val pdDoc: PDDocument = PDDocument.load(new File("test/RandomTTestPapers/bmj4_10_e005413.full.pdf"))
//    val objectExtractor: ObjectExtractor = new ObjectExtractor(pdDoc)
//    val page: Page = objectExtractor.extract(3)
//    val tables: util.List[tabulaTable] = bae.extract(page)
//    tables.foreach(table => {
//      info("Row: " + table.getRows.get(0).get(0).getText)
//    })
//    info("Fuck this shit")
//  }

  test("Get pages containing a table"){
    val PdfPath = "test/RandomTTestPapers"
    val paperTablePages = mutable.Map.empty[String,List[Int]]

    val testListRegexTable = mutable.MutableList(
      new Regex("[Tt]able.{0,60}([Cc]haracteristic|[Bb]aseline|[Pp]atient|[Pp]articipant|[Ss]ubject" +
        "|[Dd]emographic|[Ss]tudy\\s*population)"),
      new Regex("[Tt]able.{0,60}[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"))
    val files = getListOfFiles(PdfPath)
    for(file <- files){
      val pageIndices = mutable.ListBuffer.empty[Int]
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        val pdfText = convertPDFtoText(fileString)
        for(index <- 0 to pdfText.length-1){
          for(regex <- testListRegexTable){
            if(regex.findAllIn(pdfText(index)).nonEmpty){
              pageIndices += index
            }
          }
        }
        paperTablePages += fileString -> pageIndices.toList
      }
    }
    paperTablePages.foreach(entry => info("Paper: " + entry._1 + "========> Indices: " + entry._2))
  }
//  test("Tesseract OCR Test"){
//    val image = new File("test/TestImages/testSampleSizeSummary.tif")
//    val tesseract = new Tesseract()
//    try {
//      val output = tesseract.doOCR(image)
//      info("Tesseract output: " + output)
//
//    } catch {
//      case e: IOException => e.printStackTrace()
//      case b: TesseractException => b.printStackTrace()
//    }
//
//  }

  test("Extract t-test papers from whole corpus"){
    val methodSource = Source.fromFile("statterms/templates/followup/methods.csv")
    val testPermutations : ArrayBuffer[String] = new ArrayBuffer[String]()
    for (line <- methodSource.getLines()){
      if(line.split(";")(0).contains("test")){
        testPermutations += line.split(";")(0)
        line.split(";")(1).split(",").foreach(perm => {
          testPermutations += perm
        })
      }
    }

    val JournalPath = "F:\\Dropbox\\Dropbox\\all papers"
    val baseDirectory = new File(JournalPath)
    val allFiles = recursiveListFiles(baseDirectory)

    val csv_writer_corpus = new CSVWriter(new FileWriter("test/PDFLib/ttestPapersWholeCorpus.txt"))
    val csv_entry = List[String]("PDF_Name","Match")
    try {
      csv_writer_corpus.writeRow(csv_entry)
      val forLoop = new Breaks
      for(file <- allFiles){
        forLoop.breakable{
          //          if(FilenameUtils.getExtension(file.toString).equals("pdf") &&
          //            listTTestPapers.count(pap => {
          //              pap.contains(FilenameUtils.getBaseName(file.toString).substring(0,pap.length-2))}
          //            ) == 0){
          if(FilenameUtils.getExtension(file.toString).equals("pdf")){
            val pdfText = convertPDFtoText(file.toString)
            for(perm <- testPermutations){
              if(pdfText.mkString.contains(" " + perm + " ")){
                val csv_row = List[String](file.toString,perm)
                csv_writer_corpus.writeRow(csv_row)
                forLoop.break()
              }
            }
          }
        }
      }
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      if(csv_writer_corpus!=null){
        csv_writer_corpus.close()
      }
    }
  }

  test("OpenNlpTest"){
//    info("teesting open nlp")
//    val modelIn = new FileInputStream("app/lang_eng/en-sent.bin")
//    val tokenIn = new FileInputStream("app/lang_eng/en-token.bin")
//
//    try {
//      val model = new SentenceModel(modelIn)
//      val sentenceDetector = new SentenceDetectorME(model)
//      val tokenizer = new TokenizerME(tokenIn)
//      val tokens = tokenizer.tokenize("this is a sample input")
//      for(token <- tokens){
//        info("token: " + token)
//      }
//
//      val PdfText = Source.fromFile("test/TestPDFs/bmj4_1_e003824.full.pdf.txt").mkString
//      val sentences = sentenceDetector.sentDetect("First sentence. Secon crap - but maybe not hah?" +
//        "third sentence with 13.245 idiots.")
//      for(sent <- sentences){
//        info("sent: " + sent)
//      }
//    }
//    catch {
//      case e: IOException => e.printStackTrace()
//    }
//    finally {
//      if (modelIn != null) {
//        try {
//          modelIn.close()
//        }
//        catch{
//          case e: IOException => e.printStackTrace()
//        }
//      }
//    }
  }

  test("KC_Task1_one_SS_per_t-test"){
    info("Test Kuersat Classifier Full Corpus...")
    val PdfPath = "test/TestPDFs"

    val JournalPath = "F:\\Dropbox\\Dropbox\\all papers"
    val testListRegexNonOverfitted = mutable.MutableList(
      new Regex("\\b(study|sample)\\b\\D{0,15}\\d+\\D{0,15}"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\s*were\\s*assigned\\s*to"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}individuals"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,30}adults"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,25}recruited"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*\\D{0,15}"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("\\b(\\D{0,15})\\b(enrolled)\\s*\\d+([,\\s*]\\d{3})*\\D{0,15}"),
      new Regex("\\s*data\\D{0,10}of\\D{0,5}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,10}from\\D{0,5}\\d+([,\\s*]\\d{3})*"))
    val patternMatchesInGT = mutable.Map.empty[Regex, Int] // #Matches per Pattern in the Ground Truth
    val bufferedSource = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")

    for(regex <- testListRegexNonOverfitted){
      patternMatchesInGT(regex) = 0
    }

    //    var counter = 2
    for(line <- bufferedSource.getLines()){
      //      info("counter: " + counter.toString)
      val cols = line.split(",").map(_.trim)
      for(regex <- testListRegexNonOverfitted){
        if(regex.findAllIn(cols(5)).matchData.nonEmpty){
          patternMatchesInGT.update(regex,patternMatchesInGT(regex)+1)
        }
      }
      //      counter += 1
    }
    bufferedSource.close

    val files = getListOfFiles(PdfPath)
    val baseDirectory = new File(JournalPath)
    val allFiles = recursiveListFiles(baseDirectory)
    val patternMatchesTotal = mutable.Map.empty[Regex, Int]
    val patternMatchesFiltered = mutable.Map.empty[Regex, Int]
    //    testListRegexNonOverfitted.par.foreach(r => patternMatchesTotal(r) = 0)
    //    testListRegexNonOverfitted.par.foreach(r => patternMatchesFiltered(r) = 0)
    for(regex <- testListRegexNonOverfitted){
      patternMatchesTotal(regex) = 0
      patternMatchesFiltered(regex) = 0
    }
    val matchesKuersatClassifier = new ListBuffer[String]()
    //    val KuersatClassifierMap = mutable.Map.empty[String,String]

    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
    val csv_writer = new CSVWriter(new FileWriter("test/PDFLib/KC_Task1_one_SS_per_t-test.csv"))
    val CsvHeader = List[String]("PDF_Name","T-Test Match","Distance","Sample Size Match","Sample Size")
    csv_writer.writeRow(CsvHeader)
    val methodSource = Source.fromFile("statterms/templates/followup/methods.csv")
    val testPermutations : ArrayBuffer[String] = new ArrayBuffer[String]()
    for (line <- methodSource.getLines()){
      if(line.split(";")(0).contains("test")){
        testPermutations += line.split(";")(0)
        line.split(";")(1).split(",").foreach(perm => {
          testPermutations += perm
        })
      }
    }

    for (file <- files){
      val matchesInFile = new ListBuffer[String]()
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        val pdfText = convertPDFtoText(fileString)
        for(regex <- testListRegexNonOverfitted){
          if(regex.findAllIn(pdfText.mkString).nonEmpty){
            patternMatchesTotal.update(regex,patternMatchesTotal(regex)+regex.findAllIn(pdfText.mkString).length)
            patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)+regex.findAllIn(pdfText.mkString).length)
            val totalMatches = regex.findAllIn(pdfText.mkString).matchData
            while(totalMatches.hasNext){
              val currentMatch = totalMatches.next().toString()
              matchesKuersatClassifier += currentMatch
              matchesInFile += currentMatch
              if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.contains("%")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(bracketList.exists(currentMatch.contains(_))){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[-]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\s*[,]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
                "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
                "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
                "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(currentMatch.matches("\\d+\\D*\\s+[Hh]ow\\s+\\D*|\\d\\D*s+[Aa]lthough\\D*|\\d+\\D*\\s+[Tt]he\\s+\\D*" +
                "|\\d+\\D*\\s+[Mm]any\\s+\\D*|\\d+\\D*\\s+[Oo]ften\\s+\\D*|\\d+\\D*\\s+found\\s+\\D*|\\d+\\D*\\s+[Oo]n\\s+\\D*" +
                "|\\d+\\D*[Aa]mong\\D*|\\d+\\D*[Ss]how\\D*|\\d+\\D*[Tt]han\\D*|\\d+\\D*[Pp]ercent\\D*|\\d+\\D*[Ww]hich\\D*" +
                "|\\d+\\D*[Dd]uring\\D*|\\d+\\D*as\\s*well\\s*as\\D*|\\d+\\D*[Aa]nother\\D*|\\d+\\D*[Cc]haracteristics\\D*" +
                "|\\d+\\D*[Aa]ttribute\\D*|\\d+\\D*[Mm]ost\\D*|\\d+\\D*\\s+[Bb]ut\\s+\\D*|\\d+\\D*[Aa]bout\\D*" +
                "|\\d+\\D*[Ww]hether\\D*|\\d+\\D*Number\\D*") &&
                !currentMatch.matches("\\d+\\D*\\s+[Oo]f\\s*the\\s+\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              } else if(!CharMatcher.ASCII.matchesAllOf(currentMatch)){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
                matchesKuersatClassifier -= currentMatch
                matchesInFile -= currentMatch
              }
            }
          }
        }
        var testPositionMap = mutable.Map.empty[String,Int]
        var containsTest = false
        for(perm <- testPermutations){
          if(pdfText.mkString.contains(" " + perm + " ")){
            containsTest = true
            val position = pdfText.mkString.indexOf(" " + perm + " ")
            testPositionMap += " "+perm+" " -> position
          }
        }
        for(entry <- testPositionMap){
          var ss_distances = mutable.Map.empty[String,Int]
          for(matches <- matchesInFile){
            val currentDistance = Math.abs(pdfText.mkString.indexOf(matches) - entry._2)
            ss_distances += matches -> currentDistance
          }
          val minDistance = ss_distances.values.min
          val csvEntry = List[String](FilenameUtils.getBaseName(fileString),entry._1.replaceAll("\\n|\\r"," "),
            minDistance.toString,ss_distances.find(_._2==minDistance).getOrElse("foo",-1)._1,
            ss_distances.find(_._2==minDistance).getOrElse("foo",-1)._1.replaceAll("\\D+",""))
          csv_writer.writeRow(csvEntry)
        }
//        for(matches <- matchesInFile.distinct){
//          if(containsTest){
//            var distances = mutable.ListBuffer.empty[Int]
//            testPositionMap.values.par.foreach(pos => {
//              val currentDistance = Math.abs(pdfText.mkString.indexOf(matches) - pos)
//              distances += currentDistance
//            })
//            val minDistance = distances.min
//            var minDistancePermutation = "foo"
//            for(entry <- testPositionMap){
//              if(Math.abs(pdfText.mkString.indexOf(matches) - entry._2) == minDistance){
//                minDistancePermutation = entry._1
//              }
//            }
//            //        val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "))
//            val csvEntry = List[String](FilenameUtils.getBaseName(fileString),matches.replaceAll("\\n|\\r"," "),
//              matches.replaceAll("\\n|\\r"," ").replaceAll("\\D+",""),minDistance.toString)
//            csv_writer.writeRow(csvEntry)
//            //        writer.writeRow(csvEntry)     .replaceAll("\\D+",""))
//          }
//        }
      }
    }
    csv_writer.close()
    //    writer.close()

    //    info("Pattern Matches in GT")
    //    for(entry <- patternMatchesInGT){
    //      info("%-60s ==> %s".format(entry._1.toString(),entry._2).toString)
    //    }
    //    info("===============================================================================")
    //    info("Pattern Matches Total")
    //    for(entry <- patternMatchesTotal){
    //      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternMatchesFiltered(entry._1)).toString)
    //    }
    //
    //    val patternPrecision = mutable.Map.empty[Regex,Float]
    //    val patternPrecisionFiltered = mutable.Map.empty[Regex,Float]
    //    for(regex <- testListRegexNonOverfitted){
    //      patternPrecision(regex) = patternMatchesInGT(regex).toFloat / patternMatchesTotal(regex)
    //      patternPrecisionFiltered(regex) = patternMatchesInGT(regex).toFloat / patternMatchesFiltered(regex)
    //    }
    //
    //    info("===============================================================================")
    //    info("Pattern Precision")
    //
    ////    val pw = new PrintWriter(new File("test/PDFLib/Pattern_Precision_Filtering.txt"))
    //    for(entry <- patternPrecision){
    //      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
    ////      pw.write("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
    ////      pw.write("\n")
    //    }
    ////    pw.close()
    //    info("KuersatClassifier # Matches: " + matchesKuersatClassifier.length)
    ////    matchesKuersatClassifier.par.foreach(m => info("Match: " + m))
    //    info("KuersatClassifier distincst: " + matchesKuersatClassifier.distinct.length)
    //    for(matches <- matchesKuersatClassifier.distinct){
    //      info("Current Match: " + matches.replaceAll("\\n|\\r"," "))
    //    }
    //
    //    info("Recall Calculation")
    //    val paperMatch = mutable.Map.empty[String,Boolean]
    //    for(file <- files){
    //      val fileString = file.toString
    //      if(FilenameUtils.getExtension(fileString).equals("txt") && FilenameUtils.getName(fileString) != "sampleSizesExtracted"){
    //        paperMatch(FilenameUtils.getName(fileString).replace(".pdf.txt",".txt")) = false
    //      }
    //    }
    //
    //    val foundMatchesInGT = new ListBuffer[String]()
    //    val bufferedSourceSecond = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")
    //    for(line <- bufferedSourceSecond.getLines()){
    //      val cols = line.split(",").map(_.trim)
    //      for(matches <- matchesKuersatClassifier.distinct){
    //        if(cols(5).toLowerCase.replaceAll("\\s","").contains(matches.toLowerCase.replaceAll("\\s",""))){
    //          info("Found: " + cols(5))
    //          paperMatch(cols(0)) = true
    //          foundMatchesInGT += cols(5)
    //        }
    //      }
    //    }
    //    bufferedSourceSecond.close
    //    var papermatch = 0
    //    for(papermatches <- paperMatch){
    //      if(papermatches._2){
    //        papermatch += 1
    //        info("Matched Paper: " + papermatches._1)
    //      }
    //    }
    //    info("TotalFound: " + foundMatchesInGT.distinct.length)
    //    info("TotalPaperMatches: " + papermatch)
  }

  test("Mismatch Filter"){
    val bufferedSource = Source.fromFile("test/PDFLib/KuersatClassifier_fullMismatches.txt")
    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
    for(line <- bufferedSource.getLines()){
//      val mismatch = line.split("Match:\\s+")(1)
      val mismatch = line
      if(mismatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*")){
//        info("With years etc. " + mismatch)
      }
      if(mismatch.contains("%")){
//        info("With percentage: " + mismatch)
      }
      if(mismatch.matches("\\d+\\D*[.]\\D*")){
//        info("With sentence: " + mismatch)
      }
      if(mismatch.matches("\\d+[/)\\]\\(]\\D*")){
//        info("With brackets: " + mismatch)
      }
      if(bracketList.exists(mismatch.contains(_))){
//        info("With bracketList: " + mismatch)
      }
      if(mismatch.matches("\\d+\\s*[-]\\D*")){
//        info("With Dash: " + mismatch)
      }
      if(mismatch.matches("\\d+\\s*[,]\\D*")){
//        info("With Comma: " + mismatch)
      }
      if(mismatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
        "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
        "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
        "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*")){
//        info("With connection: " + mismatch)
      }
      if(mismatch.matches("\\d+\\D*\\s+[Hh]ow\\s+\\D*|\\d\\D*s+[Aa]lthough\\D*|\\d+\\D*\\s+[Tt]he\\s+\\D*" +
        "|\\d+\\D*\\s+[Mm]any\\s+\\D*|\\d+\\D*\\s+[Oo]ften\\s+\\D*|\\d+\\D*\\s+found\\s+\\D*|\\d+\\D*\\s+[Oo]n\\s+\\D*" +
        "|\\d+\\D*[Aa]mong\\D*|\\d+\\D*[Ss]how\\D*|\\d+\\D*[Tt]han\\D*|\\d+\\D*[Pp]ercent\\D*|\\d+\\D*[Ww]hich\\D*" +
        "|\\d+\\D*[Dd]uring\\D*|\\d+\\D*as\\s*well\\s*as\\D*|\\d+\\D*[Aa]nother\\D*|\\d+\\D*[Cc]haracteristics\\D*" +
        "|\\d+\\D*[Aa]ttribute\\D*|\\d+\\D*[Mm]ost\\D*|\\d+\\D*\\s+[Bb]ut\\s+\\D*|\\d+\\D*[Aa]bout\\D*" +
        "|\\d+\\D*[Ww]hether\\D*|\\d+\\D*Number\\D*") &&
       !mismatch.matches("\\d+\\D*\\s+[Oo]f\\s*the\\s+\\D*")){
//        info("With connection SECOND: " + mismatch)
      }
      if(!CharMatcher.ASCII.matchesAllOf(mismatch)){
//        info("with asciii: " + mismatch)
      }
//      info(mismatch.replaceAll("\\D+",""))
    }
  }

  test("t-test papers"){
    val PdfPath = "test/TestPDFs/T_Test"
    val files = getListOfFiles(PdfPath)
    for(file <- files){
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")) {
        val pdfText = convertPDFtoText(fileString)
      }
    }
  }

  test("AddPatternToGroundTruth"){
    val testListRegexNonOverfitted = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      //      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}men"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("[a]*[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("\\s*data\\D{0,10}of\\D{0,5}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,10}from\\D{0,5}\\d+([,\\s*]\\d{3})*"))
//    val bufferedSourceSecond = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes_WithPatterns.csv")
    val reader = new CSVReader(new FileReader("test/PDFLib/PDFLibrary_SampleSizes_WithPatterns.csv"))
    val writer = new CSVWriter(new FileWriter("test/PDFLib/PDFLibrary_SampleSizes_WithPatterns_output.csv"))
    var entries = new Array[String](6)
    while ((entries = reader.readNext()) != null){
      var list = scala.collection.mutable.ArrayBuffer.empty[String]
      for(element <- entries){
        list += element
      }
      if(list(0).equals("PDF_Name")){
        list += "MatchingPattern"
      } else {
//        for(regex <- testListRegexNonOverfitted){
//          if(regex.findAllIn(list(5)).matchData.nonEmpty){
//            list += regex.toString()
//          }
//        }
        val forLoop = new Breaks
        forLoop.breakable{
          for(regex <- testListRegexNonOverfitted){
            if(regex.findAllIn(list(5)).matchData.nonEmpty){
              list += regex.toString()
              forLoop.break()
            }
          }
        }
//        val forLoop = new Breaks
//        forLoop.breakable{
//          for(regex <- testListRegexNonOverfitted){
//            if(list(5).matches(regex.toString())){
//              list += regex.toString()
//              forLoop.break()
//            }
//          }
//        }
        if(list.length == 6){
          list += "noPattern"
        }
      }
      info("ID: " + list(1))
      writer.writeRow(list)
    }
  }

  test("t-test analysis"){
    val PdfPath = "test/TestPDFs"
    val methodsPath = "statterms/templates/followup/methods.csv"
    val files = getListOfFiles(PdfPath)
    val allPaper = new PDFLoader(new File(PdfPath)).papers

//    allPaper.par.foreach(paper => {
//      val searcher = new StatTermSearcher(paper,mockDB,papers)
//      val terms = searcher.terms
//      info("Paper: " + paper.name)
//      terms.foreach(term => info("term: " + term))
//    })

//    val searcher = mock[StatTermSearcher]
//    info("searcher: " + searcher.terms)

//    val matcher = new StatTermSearcher()

//    val bufferedSource = Source.fromFile("statterms/templates/followup/methods.csv")
//    val testPermutations : ArrayBuffer[String] = new ArrayBuffer[String]()
//    for (line <- bufferedSource.getLines()){
//      if(line.split(";")(0).contains("test")){
//        testPermutations += line.split(";")(0).replaceAll("\\s+","").toLowerCase()
//        (line.split(";")(1).split(",")).foreach(perm => {
//          testPermutations += perm.replaceAll("\\s+","").toLowerCase()
//        })
//      }
//    }
//    for(file <- files){
//      val fileString = file.toString
//      if(FilenameUtils.getExtension(fileString).equals("pdf")) {
//        val pdfDoc = PDDocument.load(new File(fileString))
//        val pdfText = convertPDFtoText(fileString).mkString.replaceAll("\\s+","").toLowerCase()
//        for(test <- testPermutations){
//          if(pdfText.contains(test)){
//            info("Found Paper: " + fileString)
//          }
//        }
//      }
//    }
  }

  test("Test Regex Precision old"){
    info("Test Regex Precision...")
    val PdfPath = "test/TestPDFs"

    val testListRegex = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}men"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}children"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}residents"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}students"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}respondents"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}adults"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}procedures"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}volunteers"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}employees"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}users"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}individuals"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}managers"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}firms"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}establishments"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("sample\\s*of\\s*\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      //      new Regex("sample\\D{0,10}included\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s?cohort\\D{0,20}of\\D{0,15}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,25}recruited"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("enrolled\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("[Tt]otal\\s*of\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,20}\\d+([,\\s*]\\d{3})*"))
    val testListRegexNonOverfitted = mutable.MutableList(
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}women"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}men"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}persons"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}participants"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}subjects"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}people"),
      new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*"),
      new Regex("study\\s*population\\s*include[sd]\\D{0,20}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled"),
      new Regex("\\s*data\\D{0,10}of\\D{0,5}\\d+([,\\s*]\\d{3})*"),
      new Regex("\\s*data\\D{0,10}from\\D{0,5}\\d+([,\\s*]\\d{3})*"))
    val patternMatchesInGT = mutable.Map.empty[Regex, Int] // #Matches per Pattern in the Ground Truth
    val bufferedSource = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")

    for(regex <- testListRegexNonOverfitted){
      patternMatchesInGT(regex) = 0
    }

    for(line <- bufferedSource.getLines()){
      val cols = line.split(",").map(_.trim)
      for(regex <- testListRegexNonOverfitted){
        if(regex.findAllIn(cols(5)).matchData.nonEmpty){
          patternMatchesInGT.update(regex,patternMatchesInGT(regex)+1)
        }
      }
    }
    bufferedSource.close


    var matchesInPDFLib = 0

    val files = getListOfFiles(PdfPath)
    val patternMatchesTotal = mutable.Map.empty[Regex, Int]
    val patternMatchesFiltered = mutable.Map.empty[Regex, Int]
    for(regex <- testListRegexNonOverfitted){
      patternMatchesTotal(regex) = 0
      patternMatchesFiltered(regex) = 0
    }
    for (file <- files){
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        val pdfDoc = PDDocument.load(new File(fileString))
        val pdfText = convertPDFtoText(fileString)
        for(regex <- testListRegexNonOverfitted){
          if(regex.findAllIn(pdfText.mkString).nonEmpty){
            patternMatchesTotal.update(regex,patternMatchesTotal(regex)+regex.findAllIn(pdfText.mkString).length)
            patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)+regex.findAllIn(pdfText.mkString).length)
            val totalMatches = regex.findAllIn(pdfText.mkString).matchData
            while(totalMatches.hasNext){
              val currentMatch = totalMatches.next().toString()
              if(currentMatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
                "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
              } else if(currentMatch.contains("%")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
              } else if(currentMatch.matches("\\d+\\D*[.]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
              } else if(currentMatch.matches("\\d+[/)\\]]\\D*")){
                patternMatchesFiltered.update(regex,patternMatchesFiltered(regex)-1)
              }
            }
          }
        }
      }
    }
    info("Pattern Matches in GT")
    for(entry <- patternMatchesInGT){
      info("%-60s ==> %s".format(entry._1.toString(),entry._2).toString)
    }
    info("===============================================================================")
    info("Pattern Matches Total")
    for(entry <- patternMatchesTotal){
      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternMatchesFiltered(entry._1)).toString)
    }

    val patternPrecision = mutable.Map.empty[Regex,Float]
    val patternPrecisionFiltered = mutable.Map.empty[Regex,Float]
    for(regex <- testListRegexNonOverfitted){
      patternPrecision(regex) = patternMatchesInGT(regex).toFloat / patternMatchesTotal(regex)
      patternPrecisionFiltered(regex) = patternMatchesInGT(regex).toFloat / patternMatchesFiltered(regex)
    }

    info("===============================================================================")
    info("Pattern Precision")

    //    val pw = new PrintWriter(new File("test/PDFLib/Pattern_Precision_Filtering.txt"))
    for(entry <- patternPrecision){
      info("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
      //      pw.write("%-60s ==> %s // %s".format(entry._1.toString(),entry._2,patternPrecisionFiltered(entry._1)).toString)
      //      pw.write("\n")
    }
    //    pw.close()
  }

  test("Pattern Analysis"){
//    val testRegex = new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}patients")
//    val testRegex = new Regex("[Nn]\\s*=\\s*\\d+([,\\s*]\\d{3})*")
    val testRegex = new Regex("\\d+([,\\s*]\\d{3})*\\D{0,20}enrolled")
    val bufferedSource = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")

    var totalMatchesGT = 0
    for(line <- bufferedSource.getLines()){
      val cols = line.split(",").map(_.trim)
      if(testRegex.findAllIn(cols(5)).nonEmpty){
        totalMatchesGT += 1
      }
    }
    info("Matches in GT: " + totalMatchesGT)

    bufferedSource.close
    var totalMatchesCount = 0
    val PdfPath = "test/TestPDFs"
    val files = getListOfFiles(PdfPath)
    for (file <- files){
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        val pdfDoc = PDDocument.load(new File(fileString))
        val pdfText = convertPDFtoText(fileString)
        val totalMatchesInPDF = testRegex.findAllIn(pdfText.mkString).matchData
        totalMatchesCount += testRegex.findAllIn(pdfText.mkString).length
        while(totalMatchesInPDF.hasNext){
          val currentMatch = totalMatchesInPDF.next().toString()
//          info("Current Match: " + currentMatch.toString())
          info("CurrentMatch: " + currentMatch)
        }
      }
    }
    info("Precision: " + totalMatchesGT.toFloat/totalMatchesCount)
  }

  test("SampleSize CSV Reader"){
    info("PDF_Name, ID, Index, part_of, N, Comment")
    val bufferedSource = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes.csv")
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      info(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}|${cols(4)}|${cols(5)}")
    }
    bufferedSource.close
  }

  test("GetConfidencePerPattern"){
    info("Running...")
    val paperDir = "test/TestPDFs/2004_12404.pdf"
    val pdfDoc = PDDocument.load(new File(paperDir))
//    val pdfText = convertPDFtoText(paperDir)
    val statChecker = StatChecker

    val pdfText = new ListBuffer[String]()
    pdfText += "hundred women"
    val patternMap = statChecker.extractSampleSizeFromPaper(pdfText.toList)
    info("PatternMap size: " + patternMap.size)
//    val confidenceMap = statChecker.getCondifence(patternMap)
//    info("Confidence Map Size: " + confidenceMap.size)
  }

  test("Get Index of Sample Size"){
    var file = new File("test/TestPDFs/2010_2107.pdf")
    var fileString = file.toString
    val pdfText = convertPDFtoText(fileString)

    val input = "362  patients"

    for(page <- pdfText.indices){
      if(pdfText(page).toLowerCase.contains(input.toLowerCase)){
        info("Index: " + page + " : " + pdfText(page).toLowerCase.indexOf(input.toLowerCase))
      }
    }
  }

  test("Recognize Number from String"){
    val statChecker = StatChecker
    info("Result: " + statChecker.recognizeDigit("eighty-four"))
  }

  test("Get SampleSize of Library"){
    val lines = Source.fromFile("test/TestPDFs/sampleSIzesExtracted.txt").getLines()
    var sampleSizePerPaper = mutable.Map.empty[String, String]

    for(line <- lines){ // build map of paper and sample size from PDF library
      val splittedLine = line.split(":")
//      info("LINE: " + line)
      sampleSizePerPaper(splittedLine(0)) = splittedLine(1)
    }

    val totalPapers = sampleSizePerPaper.keySet.size

    val PdfPath = "test/TestPDFs"
    val files = getListOfFiles(PdfPath)
    var correctFindings = mutable.Map.empty[String, Boolean] // count of how many PDFs the correct sample size is included in matches

    var countPDFs = 0
    info("PDF Count: " + countPDFs)
//    correctFindings foreach(finding => info(finding._1 + "==>" + finding._2.toString))
    correctFindings foreach(finding => info("%-60s ==> %s".format(finding._1,finding._2.toString)))
  }

  test("Regex Testings"){
    val Label_Source = Source.fromFile("test/PDFLib/PDFLibrary_SampleSizes_copy_full_labels.csv")

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
      if(cols(6).contains("OL1") || cols(6).contains("NL1") || cols(6).contains("L2")){
        if(!list_tTestPapers.contains(cols(0))){
          list_tTestPapers += cols(0)
        }
      }
    }

    val PdfPath = "test/TestPDFs"
    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
    val bracketList_NL1: List[String] = List("[","]",":","/","+","-",";","*")
    val bracketList_L2: List[String] = List("[","]",":","+","-",";","*")
//    val bracketList_OL1: List[String] = List("(",")","[","]",":","/","+",";","*")
    val files = getListOfFiles(PdfPath)
    var filesCount = 0

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

            /*L2-Pool: Filter Matches of the form 'X/Y' or 'X of Y'*/
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
            poolGroupSS = poolGroupSS.filterNot(ss => ss.contains(",") || ss.contains(":"))


//            matchesInitialSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "InitialSS" + " ==> " + ss))
//            poolInitialSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolInitialSS" + " ==> " + ss))
//            matchesActualSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "ActualSS" + " ==> " + ss))
//            poolActualSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolActualSS" + " ==> " + ss))
//            matchesActualSSNegative.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "ActualSSNegative" + " ==> " + ss))
//            poolActualSSNegative.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolActualSSNegative" + " ==> " + ss))
            matchesGroupSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "GroupSS" + " ==> " + ss))
            poolGroupSS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "PoolGroupSS" + " ==> " + ss))
//            matchesUsualKC.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "UsualKC" + " ==> " + ss))
//            info("SS_POS_Map: FILE: " + FilenameUtils.getBaseName(fileString))
//            for(i <- 0 to SS_Pos_Map.size-1){
//              info("Match: " + SS_Pos_Map(i) + " ==> "+i)
//            }
//            L2_SS.foreach(ss => info(FilenameUtils.getBaseName(fileString) + " ==> "+ "L2_SS" + " ==> " + ss))
          }
        }
      }
    }
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

  test("Regex Matching"){
    val statChecker = StatChecker
    val groupSupport = statChecker.getRelativeGroupSupport
//    mutable.ListMap(groupSupport.toSeq.sortBy(_._1):_*) foreach(entry => info(entry._1 + " ==> " + entry._2))
    groupSupport foreach(entry => info(entry._1 + " ==> " + entry._2))

    info("group index: " + statChecker.getGroupIndexPerMatch("23,948 men"))


//    val lines = Source.fromFile("test/TestPDFs/2011_2213.pdf.txt").getLines().mkString
//    var sampleSizes = Source.fromFile("test/TestPDFs/sampleSizesExtracted.txt").getLines().toList
//    val regex = new Regex("(\\d+\\D{0,20}\\s+women)|(\\d+\\D{0,20}participants)|(\\d+\\D{0,30}patients)")
//    var groupSupport = mutable.Map.empty[Int, Int] // map of occurence of match per group
//    var groupSupportRelative = mutable.Map.empty[Int, Float]
//    var sampleSizeList = mutable.MutableList[String]()
//
//    for(line <- sampleSizes){
//      sampleSizeList += line.split(":")(1)
//    }
//
//    for (i <- 1 to 3){
//      groupSupport(i) = 0
//      for (sampleSize <- sampleSizeList){
//        val matches = regex.findAllIn(sampleSize).matchData
//        while (matches.hasNext){
//          val currentMatch = matches.next()
//          if (currentMatch.group(i) != null){
//            groupSupport(i) += 1
//          }
//        }
//      }
//    }
//
//    groupSupport foreach(entry => info(entry._1 + " ==> " + entry._2))
//    val highestSupport = groupSupport.values.max
//    info("HighestSupport: " + highestSupport)
//    info("========Relative Support=========")
//    for (j <- 1 to 3){
//      groupSupportRelative(j) = groupSupport(j).toFloat/highestSupport
//    }
//    groupSupportRelative foreach(entry => info(entry._1 + " ==> " + entry._2))
  }

  test("Read File"){
    val lines = Source.fromFile("test/TestPDFs/sampleSIzesExtracted.txt").getLines()
    var regexContext = mutable.Map.empty[String, String]
    for(line <- lines){
      val splittedLine = line.split(":")
      regexContext(splittedLine(0)) = splittedLine(1)
    }
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
