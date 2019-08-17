import java.io.File

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

case class Bigrams(bigrams: BigramsMap) { // TODO: Find out why so slow

  def extractStatistics(path: String): Bigrams = {
    val entry: File = new File(path)
    if (entry.exists && entry.isDirectory) {
      println("Extracting bigrams from " + entry.getPath + "/")
      val bigramsFromDir: BigramsMap = entry
        .listFiles
        .filter(file => file.isFile && file.getName.endsWith(".sgm"))
        .map(Bigrams.getBigramsFrom)
        .foldLeft(BigramsMap())(Bigrams.merge)
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

  def getFreqsInOrderOfFrequency(word: String): Option[List[(String, Int)]] = {
    bigrams.getInOrderOfFrequency(word)
  }

  def mergeIn(bigramsIn: BigramsMap): Bigrams = {
    Bigrams(Bigrams.merge(bigrams, bigramsIn))
  }
}

object Bigrams {

  val BODY: Regex = "(?s).*<BODY>(.*)</BODY>(?s).*".r

  def apply(): Bigrams = {
    new Bigrams(BigramsMap())
  }

  def fromPath(path: String): Bigrams = {
    new Bigrams(BigramsMap()).extractStatistics(path)
  }

  // Return a list with the markup for each article
  @tailrec
  def readArticles(remainingLines: List[String], acc: List[String]): List[String] = {
    if (remainingLines.size == 1) (acc.head + "\n" + remainingLines.head) +: acc.tail
    else {
      val nextLine = remainingLines.head
      if (nextLine.startsWith("<REUTERS ")) readArticles(remainingLines.tail, nextLine +: acc)
      else readArticles(remainingLines.tail, (acc.head + "\n" + nextLine) +: acc.tail)
    }
  }

  def addBigramsFrom(tokens: List[String], oldBigrams: BigramsMap): BigramsMap = {
    val bigramPairs: List[(String, String)] = Bigrams.getBigrams(tokens)
    val newBigrams: BigramsMap = getBigramsFrom(bigramPairs)
    merge(oldBigrams, newBigrams)
  }

  def merge(bigrams1: Bigrams, bigrams2: Bigrams): Bigrams = {
    Bigrams(merge(bigrams1.bigrams, bigrams2.bigrams))
  }

  def merge(bigrams1: BigramsMap, bigrams2: BigramsMap): BigramsMap = {
    BigramsMap(
    bigrams1.map ++ bigrams2.map
      .map(entry1 => entry1._1 -> (entry1._2 ++ bigrams1.getOrElse(entry1._1, Map[String, Int]())
      .map(entry2 => entry2._1 -> (entry2._2 + entry1._2.getOrElse(entry2._1, 0))))))
  }

  def getBigramsFrom(path: File): BigramsMap = {
    println("Extracting bigrams from " + path.getPath)
    val file = Source.fromFile(path)
    val fileLines: List[String] = file.getLines().toList
    val articles: List[String] = Bigrams.readArticles(fileLines.tail, List())
    val bodies: List[String] = articles.map(extractBody).filter(body => !body.isEmpty)
    val sentenceTokens: List[List[String]] = bodies.flatMap(getSentenceTokens)
    sentenceTokens.foldLeft(BigramsMap())((acc, tokens) => addBigramsFrom(tokens, acc))
  }

  def getBigramsFrom(tokens: List[(String, String)]): BigramsMap = {
    BigramsMap().addAll(tokens)
  }

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

    groupBySentenceTokens(
    separatedBySpace,
    splitAt,
    List())
      .map(sentenceTokens => sentenceTokens.init :+ sentenceTokens.last.substring(0, sentenceTokens.last.length - 1))
      .map(sentenceTokens => sentenceTokens.map(sentenceToken => sentenceToken.toLowerCase))
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

}
