package SampleSizeExtractorTest.SampleSizeExtractorTest

import java.io.File

import org.scalatest.FunSuite

/**
  * Created by Aydinli on 02.02.2017.
  */
class PDFLibraryTest extends FunSuite{
  val PdfPath = "test/TestPDFs"

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      info("Dir does not exist")
      List[File]()
    }
  }

  test("Get Files in Directory"){
    val files = getListOfFiles(PdfPath)
    files foreach(f => info(f.toString))
  }
}
