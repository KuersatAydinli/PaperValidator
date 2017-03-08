package helper.pdfpreprocessing.pdf

import java.awt.geom.Rectangle2D
import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.PDFTextStripperByArea

/**
  * Created by pdeboer on 16/10/15.
  */
class PDFTextExtractor(pdfPath: String) extends LazyLogging {
  lazy val pages: List[String] = {
    try {
      val pdDoc: PDDocument = PDDocument.load(new File(pdfPath))

      val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
      pdfHighlight.setLineSeparator(" ")
      pdfHighlight.initialize(pdDoc)

      var txt = List[String]()
      var pdfText = new StringBuilder
      for (i <- 0 to pdDoc.getNumberOfPages){
        val page: PDPage = pdDoc.getDocumentCatalog.getPages.get(i)
        val region: Rectangle2D = new Rectangle2D.Double(0,20,page.getMediaBox.getWidth,page.getMediaBox.getHeight-80)
        val regionName: String = "content"
        val stripper: PDFTextStripperByArea = new PDFTextStripperByArea()
        stripper.addRegion(regionName,region)
        stripper.extractRegions(page)
        val currentPage = stripper.getTextForRegion("content")
        pdfText ++= currentPage
      }

//      val txt: List[String] = (0 to pdDoc.getNumberOfPages).map(pdfHighlight.textCache.getText(_)).toList
      pdDoc.close()
      txt
    } catch {
      case e1: Throwable => {
        logger.error("An error occurred while extracting text from pdf ", e1)
        Nil
      }
    }
  }
}
