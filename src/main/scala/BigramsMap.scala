import scala.collection.mutable

case class BigramsMap(map: Map[String, mutable.SortedMap[String, Int]]) {
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
        BigramsMap(map.updated(first, mutable.SortedMap(second -> 1))).addAll(bigrams.tail)
      }
    }
  }

  def get(key: String): Option[mutable.SortedMap[String, Int]] = {
    map.get(key)
  }

  def getOrElse[V1 >: mutable.SortedMap[String, Int]](key: String, default: V1): V1 = {
    map.getOrElse(key, default)
  }
}

object BigramsMap {
  def apply(elems: (String, mutable.SortedMap[String, Int])*): BigramsMap = BigramsMap(Map(elems: _*))
  def apply(): BigramsMap = BigramsMap(Map[String, mutable.SortedMap[String, Int]]())
}