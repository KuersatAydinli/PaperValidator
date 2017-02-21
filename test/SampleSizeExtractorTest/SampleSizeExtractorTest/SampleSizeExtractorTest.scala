package SampleSizeExtractorTest.SampleSizeExtractorTest
import java.io._

import helper.pdfpreprocessing.pdf.PDFTextExtractor
import helper.statcheck.{Statchecker => StatChecker}
import org.apache.commons.io.FilenameUtils
import org.apache.pdfbox.pdmodel.PDDocument
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
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

  test("Test new Map Structure"){
    info("HEy yyyyy")
    val paperDir = "test/TestPDFs/2004_12404.pdf"
    val pdfDoc = PDDocument.load(new File(paperDir))
    val pdfText = convertPDFtoText(paperDir)
    val statChecker = StatChecker

    val patternMap = statChecker.extractSampleSizeFromPaper(pdfText)
    info("patternMapSize : " + patternMap.size)
    for(pattern <- patternMap){
      info("Pattern: " + pattern._1.toString + " ===> Support: " + pattern._2.support)
    }
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

  test("SampleSize CSV Reader"){
    info("PDF_Name, ID, Index, part_of, N, Comment")
    val bufferedSource = Source.fromFile("test/TestPDFs/PDFLibrary_SampleSizes.csv")
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      info(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}|${cols(4)}|${cols(5)}")
    }
    bufferedSource.close
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
    for (file <- files){
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){
        correctFindings(fileString) = false
        countPDFs += 1
        for(sampleSize <- sampleSizePerPaper.keys){
          val fileBase = FilenameUtils.getBaseName(fileString)
          if(fileBase.equalsIgnoreCase(sampleSize)){
            val pdfDoc = PDDocument.load(new File(fileString))
            val pdfText = convertPDFtoText(fileString)
            val statChecker = StatChecker
            val sampleSizeContext = statChecker.extractSampleSizeContext(pdfText)

            for(entry <- sampleSizeContext){
//              if(fileBase.equalsIgnoreCase("2006_1462")){
//                info(entry._1.replaceAll("\\s","").toLowerCase)
//                info(sampleSizePerPaper(sampleSize).replaceAll("\\s","").toLowerCase)
//                if(entry._1.replaceAll("\\s","").toLowerCase.equalsIgnoreCase(sampleSizePerPaper(sampleSize).replaceAll("\\s","").toLowerCase)){
//                  info("TRUEEEEE")
//                }
//              }


              if(entry._1.head._1.replaceAll("\\s","").toLowerCase.
                contains(sampleSizePerPaper(sampleSize).replaceAll("\\s","").toLowerCase)){
                correctFindings(fileString) = true
              }
            }
          }
        }
      }
    }
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
