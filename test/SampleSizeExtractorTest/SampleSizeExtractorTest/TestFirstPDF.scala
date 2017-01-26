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

  test("GetSampleSize"){
    val paperDir = "test/TestPDFs/manuscript4.pdf"
    val pdfDoc = PDDocument.load(new File(paperDir))
    val pdfText = convertPDFtoText(paperDir)
    assert(pdfText.length == 38)

    val statChecker = StatChecker
    val sampleSize = statChecker.extractSampleSize(pdfText)

    if (!new File("test/TestPDFs/sampleSizeString" + ".text").exists()) {
      val pw = new PrintWriter(new File("test/TestPDFs/sampleSizeString" + ".txt"))
      pw.write(sampleSize)
      pw.close()
    }
    assert(sampleSize.length > 1)
  }

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
