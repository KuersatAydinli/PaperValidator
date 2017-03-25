package lang_eng

/**
  * Created by Aydinli on 25.03.2017.
  */
object OpenNlpTagger {
  import opennlp.tools.sentdetect.{SentenceDetectorME, SentenceModel}

  lazy val sentenceDetector =
    new SentenceDetectorME(
      new SentenceModel(this.getClass.getResourceAsStream("app/lang_eng/en-sent.bin")))

  def main (args: Array[String]) {
  }

}
