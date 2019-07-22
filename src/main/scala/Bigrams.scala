import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

case class Bigrams(bigrams: Map[String, mutable.SortedMap[String, Int]]) {

  def mergeIn(bigramsIn: Map[String, mutable.SortedMap[String, Int]]): Bigrams = {
    Bigrams(Bigrams.merge(bigrams, bigramsIn))
  }

  def extractStatistics(path: String): Bigrams = {
    val entry: File = new File(path)
    if (entry.exists && entry.isDirectory) {
      println("Extracting bigrams from " + entry.getPath + "/")
      val bigramsFromDir: Map[String, mutable.SortedMap[String, Int]] = entry
        .listFiles
        .filter(file => file.isFile && file.getName.endsWith(".sgm"))
        .map(Bigrams.getBigramsFrom)
        .foldLeft(Map[String, mutable.SortedMap[String, Int]]())(Bigrams.merge)
      val bigramsFromSubDirs: Bigrams = entry
        .listFiles
        .filter(entry => entry.isDirectory)
        .map(entry => extractStatistics(entry.getAbsolutePath))
        .foldLeft(Bigrams())(Bigrams.merge)
      bigramsFromSubDirs.mergeIn(bigramsFromDir)
    } else if (entry.exists && entry.isFile) {
      Bigrams(Bigrams.getBigramsFrom(entry))
    } else
      throw new RuntimeException("Incorrect path")
  }

  def getFreqs(word: String): Option[mutable.SortedMap[String, Int]] = {
    bigrams.get(word)
  }
}

object Bigrams {

  def fromPath(path: String): Bigrams = {
    new Bigrams(Map[String, mutable.SortedMap[String, Int]]()).extractStatistics(path)
  }

  def apply(): Bigrams = {
    new Bigrams(Map())
  }

  val BODY: Regex = "(?s).*<BODY>(.*)</BODY>(?s).*".r

  // Return a list with the markup for each article
  @tailrec
  def readArticles(remainingLines: List[String], acc: List[String]): List[String] = {
    if (remainingLines.size == 1) acc
    else {
      val nextLine = remainingLines.head
      if (nextLine.startsWith("<REUTERS ")) readArticles(remainingLines.tail, nextLine +: acc)
      else readArticles(remainingLines.tail, (acc.head + "\n" + nextLine) +: acc.tail)
    }
  }

  def merge(bigrams1: Map[String, mutable.SortedMap[String, Int]],
                    bigrams2: Map[String, mutable.SortedMap[String, Int]]): Map[String, mutable.SortedMap[String, Int]] = {
    bigrams2 ++ bigrams1
      .map(entry1 => entry1._1 -> (entry1._2 ++ bigrams2.getOrElse(entry1._1, mutable.SortedMap[String, Int]())
        .map(entry2 => entry2._1 -> (entry2._2 + entry1._2.getOrElse(entry2._1, 0)))))
  }

  def merge(bigrams1: Bigrams, bigrams2: Bigrams): Bigrams = {
    new Bigrams(merge(bigrams1.bigrams, bigrams2.bigrams))
  }

  def getBigramsFrom(path: File): Map[String, mutable.SortedMap[String, Int]] = {
    val file = Source.fromFile(path)
    val fileLines: List[String] = file.getLines().toList
    val articles: List[String] = Bigrams.readArticles(fileLines.tail, List())
    val bodies: List[String] = articles.map(extractBody).filter(body => !body.isEmpty)
    val sentenceTokens: List[List[String]] = bodies.flatMap(getSentenceTokens)
    sentenceTokens.foldLeft(Map[String, mutable.SortedMap[String, Int]]())((acc, tokens) => addBigramsFrom(tokens, acc))
  }

  def getBigramsFrom(tokens: List[(String, String)]): BigramsMap = {
    BigramsMap().addAll(tokens)
  }
  /*
  [("hello", "there"), ("hello", "there"), ("hello", "you"), ("see", "you")]
  =>
  Map("hello" -> SortedMap("there" -> 2, "you" -> 1), "see" -> SortedMap("you" -> 1))
   */

  def getBigrams(tokens: List[String]): List[(String, String)] = {
    tokens.indices.
      map(i => {
        if (i < tokens.size - 1) (tokens(i), tokens(i + 1))
        else null
      })
      .filter(_ != null).toList
  }

  // Return the body of the markup of one article
  def extractBody(article: String): String = {
    try {
      val body: String = article match {
        case Bigrams.BODY(bodyGroup) => bodyGroup
      }
      body
    }
    catch {
      case _: MatchError => ""
    }
  }

  def getSentenceTokens(text: String): List[List[String]] = {
    val separatedBySpace: List[String] = text
      .replace('\n', ' ')
      .replaceAll(" +", " ") // regex
      .split(" ")
      .map(token => if (token.endsWith(",")) token.init.toString else token)
      .toList

    val splitAt: List[Int] = separatedBySpace.indices
      .filter(i => i > 0 && separatedBySpace(i - 1).endsWith(".") || i == 0)
      .toList

    groupBySentenceTokens(separatedBySpace, splitAt, List()).map(sentenceTokens => sentenceTokens.init :+ sentenceTokens.last.substring(0, sentenceTokens.last.length - 1))
  }

  @tailrec
  def groupBySentenceTokens(tokens: List[String], splitAt: List[Int], sentences: List[List[String]]): List[List[String]] = {
    if (splitAt.size <= 1) {
      if (splitAt.size == 1) {
        sentences :+ tokens.slice(splitAt.head, tokens.size)
      } else {
        sentences
      }
    }
    else groupBySentenceTokens(tokens, splitAt.tail, sentences :+ tokens.slice(splitAt.head, splitAt.tail.head))
  }

  def addBigramsFrom(tokens: List[String], bigrams: Map[String, mutable.SortedMap[String, Int]]): Map[String, mutable.SortedMap[String, Int]] = {
    var newBigrams = bigrams
    val bigramPairs: List[(String, String)] = Bigrams.getBigrams(tokens)

    bigramPairs.foreach(bigram => { // TODO: This code uses side effects to get the job done. Try to remove them.
      val currentFreqs: mutable.SortedMap[String, Int] = newBigrams.get(bigram._1)
        .map((map: mutable.SortedMap[String, Int]) => map)
        .getOrElse(mutable.SortedMap())
      val incrementedWordFreq = currentFreqs.get(bigram._2)
        .map(freq => freq + 1)
        .getOrElse(1)

      val newFreqs = currentFreqs + (bigram._2 -> incrementedWordFreq)
      newBigrams = newBigrams - bigram._1 + (bigram._1 -> newFreqs)
    })

    newBigrams
  }
}
