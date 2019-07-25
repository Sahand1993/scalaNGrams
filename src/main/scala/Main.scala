object Main extends App {

  val start = System.currentTimeMillis()
  val bigrams: Bigrams = Bigrams.fromPath("dataset/reut2-000.sgm")
  val end = System.currentTimeMillis()

  println((end - start) / 1000 / 60 + " minutes")
}
