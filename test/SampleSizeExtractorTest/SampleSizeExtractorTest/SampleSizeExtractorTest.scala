package SampleSizeExtractorTest.SampleSizeExtractorTest
import java.io._

import au.com.bytecode.opencsv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import helper.pdfpreprocessing.pdf.{PDFLoader, PDFTextExtractor}
import helper.statcheck.{Statchecker => StatChecker}
import org.apache.commons.io.FilenameUtils
import org.apache.pdfbox.pdmodel.PDDocument
import org.scalatest.FunSuite

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
              if(entry._2 == minDistance){
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

  test("Mismatch Filter"){
    val bufferedSource = Source.fromFile("test/PDFLib/PatternMismatches_3rd.txt")
    val bracketList: List[String] = List("(",")","[","]",":","/","+",";","*")
    for(line <- bufferedSource.getLines()){
      val mismatch = line.split("Match:\\s+")(1)
      if(mismatch.matches("\\d+([,\\s*]\\d{3})*\\D{0,10}years\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}months\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}days\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}hours\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}weeks\\D*|\\d+([,\\s*]\\d{3})*\\s*[h]\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}cm\\D*|\\d+([,\\s*]\\d{3})*\\D*[,]\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}km\\D*|\\d+([,\\s*]\\d{3})*\\D{0,10}kg\\D*" +
        "|\\d+([,\\s*]\\d{3})*\\D{0,10}year[-\\s]old\\D*")){
        info("With years etc. " + mismatch)
      }
      if(mismatch.contains("%")){
        info("With percentage: " + mismatch)
      }
      if(mismatch.matches("\\d+\\D*[.]\\D*")){
        info("With sentence: " + mismatch)
      }
      if(mismatch.matches("\\d+[/)\\]\\(]\\D*")){
        info("With brackets: " + mismatch)
      }
      if(bracketList.exists(mismatch.contains(_))){
        info("With bracketList: " + mismatch)
      }
      if(mismatch.matches("\\d+\\s*[-]\\D*")){
        info("With Dash: " + mismatch)
      }
      if(mismatch.matches("\\d+\\s*[,]\\D*")){
        info("With Comma: " + mismatch)
      }
      if(mismatch.matches("\\d+\\D*\\s+for\\s+\\D*|\\d\\D*+\\s+and\\s+\\D*|\\d+\\D*\\s+by\\s+\\D*" +
        "|\\d+\\D*\\s+our\\s+\\D*|\\d+\\D*\\s+between\\s+\\D*|\\d+\\D*\\s+in\\s+\\D*|\\d+\\D*\\s+from\\s+\\D*" +
        "|\\d+\\D*\\s+to\\s+\\D*|\\d+\\D*\\s+that\\s+\\D*|\\d+\\D*\\s+times\\s+\\D*|\\d+\\D*\\s+with\\s+\\D*" +
        "|\\d+\\D*\\s+when\\s+\\D*|\\d+\\D*\\s+or\\s+\\D*|\\d+\\D*\\s+while\\s+\\D*")){
        info("With connection: " + mismatch)
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
//    val regex = new Regex("(\\d+\\D{0,30}patients|\\d+[,]\\d{3}\\D{0,20}patients)")
//    val regex = new Regex("(\\d+([,\\s*]\\d{3})*\\D{0,20}patients)")
////      + "|(\\d+[,]\\d{3}\\D{0,20}women)")
//    val string = "5754 patients"
//    val matches = regex.findAllIn(string).matchData
//    while (matches.hasNext){
//      val currentMatch = matches.next()
//      info("current match: " + currentMatch)
//      info("group size: " + currentMatch.groupCount)
//    }
    val statChecker = StatChecker
    val regex = new Regex(".{0,50}women")
    //      + "|(\\d+[,]\\d{3}\\D{0,20}women)")
    val string = "jasdfie asdfk asdife women"
    val matches = regex.findAllIn(string).matchData
    while (matches.hasNext){
      val currentMatch = matches.next()
      info("current match: " + currentMatch.toString())
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

  def deleteFile(filename: String) = { new File(filename).delete() }

  def convertPDFtoText(path: String): List[String] = {
    val paperLink = path
    val text = new PDFTextExtractor(paperLink).pages
    if (!new File(paperLink + ".text").exists()) {
      val pw = new PrintWriter(new File(paperLink + ".txt"))
      pw.write(text.map(_.toLowerCase()).mkString("\n\n"))
      pw.close()
    }
    text
  }
}
