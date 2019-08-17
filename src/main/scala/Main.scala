object Main extends App {

  val start = System.currentTimeMillis()
  val bigrams: Bigrams = Bigrams.fromPath("dataset/reut2-007.sgm")
  val end = System.currentTimeMillis()

  println((end - start) / 1000 + " seconds")
  println(bigrams.getFreqsInOrderOfFrequency("first"))
}
