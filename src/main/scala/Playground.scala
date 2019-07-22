import scala.collection.mutable

object Playground extends App {

  //val bigrams: Bigrams = Bigrams.fromPath("dataset")


  /*
  [("hello", "there"), ("hello", "there"), ("hello", "you"), ("see", "you")]
  =>
  Map("hello" -> SortedMap("there" -> 2, "you" -> 1), "see" -> SortedMap("you" -> 1))
   */
  val tokens = List(("hello", "there"), ("hello", "there"), ("hello", "you"), ("see", "you"))
}
