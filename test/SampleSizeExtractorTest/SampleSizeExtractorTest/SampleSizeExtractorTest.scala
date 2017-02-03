package SampleSizeExtractorTest.SampleSizeExtractorTest
import java.io.{File, PrintWriter}

import helper.pdfpreprocessing.pdf.PDFTextExtractor
import helper.statcheck.{Statchecker => StatChecker}
import org.apache.commons.io.FilenameUtils
import org.apache.pdfbox.pdmodel.PDDocument
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.io.Source

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

    val sampleSizeContext = statChecker.extractSampleSizeContext(pdfText)
    info("PDF Name: " + paperDir)
    info("ContextMapSize : " + sampleSizeContext.size)
    sampleSizeContext foreach((entry) => info(entry._1 + " ===> " + entry._2))
    info("========================================================================================================================")
    info("========================================================================================================================")
    val filteredContext = statChecker.filterSampleSizeContext(sampleSizeContext)
    info("Size Filtered Context: " + filteredContext.size)
    filteredContext foreach((entry) => info(entry._1 + " ===> " + entry._2))
  }

  test("Get SampleSize of Library"){
    val lines = Source.fromFile("test/TestPDFs/sampleSIzesExtracted.txt").getLines()
    var sampleSizeMap = mutable.Map.empty[String, String]

    for(line <- lines){ // build map of paper and sample size from PDF library
      val splittedLine = line.split(":")
      sampleSizeMap(splittedLine(0)) = splittedLine(1)
    }

    val PdfPath = "test/TestPDFs"
    val files = getListOfFiles(PdfPath)
    for (file <- files){
      val fileString = file.toString
      if(FilenameUtils.getExtension(fileString).equals("pdf")){

        for(sampleSize <- sampleSizeMap.keys){
          val fileBase = FilenameUtils.getBaseName(fileString)
          info("File Base: " + fileBase)
          if(fileBase.equalsIgnoreCase(sampleSize)){
            val pdfDoc = PDDocument.load(new File(fileString))
            val pdfText = convertPDFtoText(fileString)
            val statChecker = StatChecker
            val sampleSizeContext = statChecker.extractSampleSizeContext(pdfText)

            for(entry <- sampleSizeContext){
              if(entry._1.replaceAll("\\s","").contains(sampleSizeMap(sampleSize).replaceAll("\\s",""))){
                info("Correct for " + fileString)
              }
            }
          }
        }
      }
    }
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
