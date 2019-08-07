object Main extends App {

  val start = System.currentTimeMillis()
  val bigrams: Bigrams = Bigrams.fromPath("dataset")
  val end = System.currentTimeMillis()

  println((end - start) / 1000 + " second")

  val list: Map[String, Int] = Map("a" -> 1, "c" -> 3, "b" -> 2)

  def insertSorted(sortedList: List[(String, Int)], entry: (String, Int)): List[(String, Int)] = { // TODO: Put inside bigrams class
    def helper(leftHalf: List[(String, Int)], rightHalf: List[(String, Int)]): List[(String, Int)] = {
      if (leftHalf.isEmpty) rightHalf :+ entry
      else if (entry._2 > leftHalf.head._2) helper(leftHalf.tail, rightHalf :+ leftHalf.head)
      else rightHalf.:+(entry).++(leftHalf)
    }
    helper(sortedList, List())
  }

  def sortOnInt(stringToInt: Map[String, Int]): List[String] = { // TODO: Put inside bigrams class
    def sort(restOfMap: Map[String, Int], acc: List[(String, Int)]): List[(String, Int)] = {
      if (restOfMap.isEmpty) acc
      else {
        val entryToSort: (String, Int) = restOfMap.head
        sort(restOfMap.tail, insertSorted(acc, entryToSort))
      }
    }
    sort(stringToInt, List[(String, Int)]()).map((tuple: (String, Int)) => tuple._1)
  }

  //println(sortOnInt(bigrams.getFreqs("he").getOrElse(Map())))
  println(sortOnInt(list))

}
