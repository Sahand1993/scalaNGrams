
object Playground extends App {

  val bigrams: Bigrams = Bigrams.fromPath("dataset")

  val map1 = Map(1 -> "one", 2 -> "two")
  val map2 = Map(1 -> "new one")
  println(map1 ++ map2)
}
