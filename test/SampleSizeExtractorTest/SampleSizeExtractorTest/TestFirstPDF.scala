package SampleSizeExtractorTest.SampleSizeExtractorTest
import helper.statcheck.{Statchecker => StatChecker}
import org.scalatest.{FunSuite, Matchers}
/**
  * Created by Aydinli on 26.01.2017.
  */
class TestFirstPDF extends FunSuite with Matchers{
  test("assertion testing"){
    val x = 3
    assert(x == 3)
  }
}
