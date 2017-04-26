package helper.statcheck

/**
  * Created by Aydinli on 23.04.2017.
  * This class handles the main logic for automatically extracting the various sample size types of a study.
  * The chosen approach is basically pattern matching using RegEx and applying of subsequent heurisics to retrieve
  * the correct sample sizes
  */
class SampleSizeExtractor(val pdfText: String) {
  /**
    * This sample size corresponds to the number of participants initially recruited for a certain study - before any exclusions
    * or assignments to one or more study arms
    */
  var initalSampleSize: Int = 0
  /**
    * This sample size corresponds to the number of participants on whom the study was actually performed - after any exclusions
    * of participants but before assignments to one or more study arms
    */
  var actualSampleSize: Int = 0
  /**
    * This sample size represents a list of number of participants in the single study arms or groups if there are any
    */
  var groupSampleSizes: List[Int] = List(0)
  var pdfString = pdfText
  /**
    * this variable contains a map of the form [SampleSizeType -> SampleSize(s)]. The key is one of the member variable sample
    * sizes
    */
//  lazy val sampleSizeMap: scala.collection.mutable.Map[String, List[Int]] = {
//    var returnMap = scala.collection.mutable.Map.empty[String, List[Int]]
//    returnMap += "initialSampleSize" -> List(this.initalSampleSize)
//    returnMap += "actualSampleSize" -> List(this.actualSampleSize)
//    returnMap += "groupSampleSizes" -> this.groupSampleSizes
//
//    val patternsInitialSampleSize = mutable.MutableList(
//      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,40}\\s*invited"),
//      new Regex("invited\\s*\\d+([,\\s*]\\d{3})*\\D{0,20}"),
//      new Regex("reviewed\\s*\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
//        "persons)"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
//        "persons)\\s*reviewed"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,40}\\s*screened"),
//      new Regex("screened\\s*\\d+([,\\s*]\\d{3})*\\D{0,20}"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
//        "persons)\\s*\\D{0,20}\\s*included"),
//      new Regex("included\\s*\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|" +
//        "persons)\\s*\\D{0,20}"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}\\s*assessed\\s*for\\s*eligibility"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\D{0,30}\\s*met\\s*\\D{0,20}\\s*criteria")
//    )
//    val patternsActualSampleSize = mutable.MutableList(
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,40}\\s*(allocated|assigned)\\s*to"),
//      new Regex("[Oo]f\\s*the\\s*\\d+([,\\s*]\\d{3})*\\D{0,25}(recruited|included)"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,20}\\s*recruited"),
//      new Regex("recruited\\s*\\d+([,\\s*]\\d{3})*"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,25}randomized"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,25}randomly\\s*(allocated|assigned)\\s*to"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,35}underwent\\s*randomization")
//    )
//    val patternsActualSampleSizeNegative = mutable.MutableList(
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,40}\\s*not\\D{0,15}\\s*eligible"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,40}\\s*(did\\s*not|didn't)\\s*meet\\D{0,15}\\s*(criteria|criterion)"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|persons)\\D{0,20}\\s*were\\s*excluded")
//    )
//    val patternsGroupSampleSize = mutable.MutableList(
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|persons)\\s*in\\s*the\\s*group"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*(people|patients|participants|subjects|cases|persons)\\s*received"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\D{0,20}\\s*controls"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*\\/\\s*\\d+([,\\s*]\\d{3})*"),
//      new Regex("\\d+([,\\s*]\\d{3})*\\s*of\\s*\\d+([,\\s*]\\d{3})*")
//    )
//  }
}
