package SampleSizeExtractorTest.SampleSizeExtractorTest
import org.scalatest.{FlatSpec, FunSuite, GivenWhenThen}
import org.scalatestplus.play._

import scala.collection.mutable
/**
  * Created by Aydinli on 26.01.2017.
  */
class SampleSizeExtractorTest extends FunSuite with GivenWhenThen{
  val x = 3
  val y = 4
  val z = x+y

  test("test addition"){
    assert(z == 7)
  }

}
