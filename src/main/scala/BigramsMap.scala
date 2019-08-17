
case class BigramsMap(map: Map[String, Map[String, Int]]) { // TODO replace with *immutable* sorted map

  def addAll(bigrams: List[(String, String)]): BigramsMap = {
    if (bigrams.isEmpty) this
    else {
      val first = bigrams.head._1
      val second = bigrams.head._2
      if (map.contains(first)) {
        if (map(first).contains(second)) {
          BigramsMap(map.updated(first, map(first) + (second -> (map(first)(second) + 1)))).addAll(bigrams.tail)
        } else {
          BigramsMap(map.updated(first, map(first) + (second -> 1))).addAll(bigrams.tail)
        }
      } else {
        BigramsMap(map.updated(first, Map(second -> 1))).addAll(bigrams.tail)
      }
    }
  }

  def getInOrderOfFrequency(key: String): Option[List[(String, Int)]] = map.get(key).map(sortOnInt)

  def getOrElse[V1 >: Map[String, Int]](key: String, default: V1): V1 = map.getOrElse(key, default)

  def insertSorted(sortedList: List[(String, Int)], entry: (String, Int)): List[(String, Int)] = {
    def helper(leftHalf: List[(String, Int)], rightHalf: List[(String, Int)]): List[(String, Int)] = {
      if (leftHalf.isEmpty) rightHalf :+ entry
      else if (entry._2 < leftHalf.head._2) helper(leftHalf.tail, rightHalf :+ leftHalf.head)
      else rightHalf.:+(entry).++(leftHalf)
    }
    helper(sortedList, List())
  }

  def sortOnInt(stringToInt: Map[String, Int]): List[(String, Int)] = {
    def sort(restOfMap: Map[String, Int], acc: List[(String, Int)]): List[(String, Int)] = {
      if (restOfMap.isEmpty) acc
      else {
        val entryToSort: (String, Int) = restOfMap.head
        sort(restOfMap.tail, insertSorted(acc, entryToSort))
      }
    }
    sort(stringToInt, List[(String, Int)]())
  }
}

object BigramsMap {
  def apply(elems: (String, Map[String, Int])*): BigramsMap = BigramsMap(Map(elems: _*))
  def apply(): BigramsMap = BigramsMap(Map[String, Map[String, Int]]())
}