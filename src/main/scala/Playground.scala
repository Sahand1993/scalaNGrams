import java.io.File

import Playground.{articles, fileLines, tokenizedLines}

import scala.io.Source
import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.collection.mutable

object Playground extends App {
  val BODY: Regex = "(?s).*<BODY>(.*)</BODY>(?s).*".r
  val TOKEN: Regex = "[^a-z]*([a-z]*).*".r
  val file = Source.fromFile("dataset/reut2-000.sgm")
  val fileLines: List[String] = file.getLines().toList

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

  // Return the body of the markup of one article
  def extractBody(article: String): String = {
    try {
      val body: String = article match {
        case BODY(bodyGroup) => bodyGroup
      }
      body
    }
    catch {
      case _: MatchError => ""
    }
  }

  def splitIntoLines(article: String): List[String] = {
    article.split("\n").toList
  }

  def tokenize(line: String): List[String] = {
    line.split(" ")
      .map(_.toLowerCase)
      .map {
          case TOKEN(cleaned) => cleaned
          case _ => null
      }
      .filter(_ != null).toList
  }

  val articles: List[String] = readArticles(fileLines.tail, List(""))
  //articles.foreach(println)
  val tokenizedLines: List[List[String]] = articles
    .map(extractBody)
    .filter(!_.equals(""))
    .flatMap(article => splitIntoLines(article))
    .map((line: String) => tokenize(line))

  /*
  Let's create some n-gram probabilities.
  Let's start with bigrams. Bigram frequencies
  can be stored as a map between a word and a
  list of words sorted on the probability of
  that word following the key-word.

  So we need a Map[String, Map[String, Int]]
   */

  def addBigramsFrom(tokens: List[String], bigrams: Map[String, mutable.SortedMap[String, Int]]): Map[String, mutable.SortedMap[String, Int]] = {
    var newBigrams = bigrams
    val bigramsFromTokens: List[(String, String)] = getBigrams(tokens)

    bigramsFromTokens.foreach(bigram => {
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

  private def getBigrams(tokens: List[String]): List[(String, String)] = {
    tokens.indices.
      map(i => {
        if (i < tokens.size - 1) (tokens(i), tokens(i + 1))
        else null
      })
      .filter(_ != null).toList
  }

  def merge(map1: Map[String, mutable.SortedMap[String, Int]],
            map2: Map[String, mutable.SortedMap[String, Int]]):
  Map[String, mutable.SortedMap[String, Int]] = {
    // TODO

  }

  def getBigramsFrom(path: String): Map[String, mutable.SortedMap[String, Int]] = {
    val path = new File(path)
    if (path.exists() && path.isDirectory) {
      path
        .listFiles
        .filter(file => file.isFile && file.getName.endsWith(".sgm"))
        .map(getBigramsFrom)
        .foldLeft(Map[String, mutable.SortedMap[String, Int]]())(merge)
    } else if (path.exists && path.isFile) {
      getBigramsFrom(path)
    } else
      throw new RuntimeException("Incorrect path")
  }

  private def getBigramsFrom(path: File): Map[String, mutable.SortedMap[String, Int]] = {
    val file = Source.fromFile(path)
    val fileLines: List[String] = file.getLines().toList
    val articles: List[String] = readArticles(fileLines.tail, List(""))
    val tokenizedLines: List[List[String]] = articles
      .map(extractBody)
      .filter(!_.equals(""))
      .flatMap(article => splitIntoLines(article))
      .map((line: String) => tokenize(line))

    tokenizedLines.foldLeft(Map[String, mutable.SortedMap[String, Int]]())((acc, tokens) => addBigramsFrom(tokens, acc))
  }

  //create bigrams for one line
  val bigrams: Map[String, mutable.SortedMap[String, Int]] = getBigramsFrom("dataset/")
  println(bigrams)
}