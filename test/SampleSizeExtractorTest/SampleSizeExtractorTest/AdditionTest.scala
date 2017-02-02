package SampleSizeExtractorTest.SampleSizeExtractorTest
import org.scalatest.FunSuite
/**
  * Created by Aydinli on 26.01.2017.
  */
class AdditionTest extends FunSuite{
  val x = 3
  val y = 4
  val z = x + y

  test("test addition"){
    assert(z == 7)
  }

  test("second test"){
    assert(z +1 == 8)
  }
}
