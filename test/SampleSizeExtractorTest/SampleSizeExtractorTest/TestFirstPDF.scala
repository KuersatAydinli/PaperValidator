package SampleSizeExtractorTest.SampleSizeExtractorTest
import java.io.{File, PrintWriter}

import helper.pdfpreprocessing.pdf.PDFTextExtractor
import helper.statcheck.{Statchecker => StatChecker}
import org.apache.pdfbox.pdmodel.PDDocument
import org.scalatest.FunSuite

/**
  * Created by Aydinli on 26.01.2017.
  */
class TestFirstPDF extends FunSuite{

  val paperDir = "test/TestPDFs/manuscript4.pdf"
  val pdfDoc = PDDocument.load(new File(paperDir))
  val pdfText = convertPDFtoText(paperDir)
  val statChecker = StatChecker

  test("GetSampleSize"){
    assert(pdfText.length == 38)
    val sampleSize = statChecker.extractSampleSize(pdfText)

    if (new File("test/TestPDFs/sampleSizeString" + ".text").exists()){
      deleteFile("test/TestPDFs/sampleSizeString" + ".txt")
    }

    if (!new File("test/TestPDFs/sampleSizeString" + ".text").exists()) {
      val pw = new PrintWriter(new File("test/TestPDFs/sampleSizeString" + ".txt"))
      pw.write(sampleSize)
      pw.close()
    }

    info("Sample Size: " + sampleSize)
    assert(sampleSize.length > 1)
  }

  test("Get SampleSize Context"){
    val sampleSizeContext = statChecker.extractSampleSizeContext(pdfText)
    info("ContextMapSize : " + sampleSizeContext.size)
    sampleSizeContext foreach((t2) => info(t2._1 + "-->" + t2._2))
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
