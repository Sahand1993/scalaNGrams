import java.io.File

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.io.Source

class BigramsTest extends FunSuite {
  test("Bigrams.testMerge") {
    val map1 = BigramsMap("hi" -> mutable.SortedMap("again" -> 10))
    val map2 = BigramsMap("hi" -> mutable.SortedMap("there" -> 1), "my" -> mutable.SortedMap("god" -> 10))
    val mapmerged = Bigrams.merge(map1, map2)
    assert(mapmerged == BigramsMap("hi" -> mutable.SortedMap("again" -> 10, "there" -> 1), "my" -> mutable.SortedMap("god" -> 10)))
  }

  test("Bigrams.extractStatistics") {
    // TODO, maybe
  }

  test("Bigrams.readArticles") {
    val file = Source.fromFile("src/test/resources/mini.sgm")
    val fileLines: List[String] = file.getLines().toList
    val articles: List[String] = Bigrams.readArticles(fileLines.tail, List())
    assert(articles.size == 1)
    assert(articles.head == "<REUTERS TOPICS=\"YES\" LEWISSPLIT=\"TEST\" CGISPLIT=\"TRAINING-SET\" OLDID=\"5429\" NEWID=\"15531\">\n<DATE> 9-APR-1987 09:40:15.27</DATE>\n<TOPICS><D>grain</D><D>ship</D></TOPICS>\n<PLACES><D>uk</D></PLACES>\n<PEOPLE></PEOPLE>\n<ORGS></ORGS>\n<EXCHANGES></EXCHANGES>\n<COMPANIES></COMPANIES>\n<UNKNOWN>\n\n&#5;&#5;&#5;C G\n&#22;&#22;&#1;f0885&#31;reute\nu f BC-LONDON-FREIGHT-MARKET   04-09 0100</UNKNOWN>\n<TEXT>&#2;\n<TITLE>LONDON FREIGHT MARKET FEATURES GRAIN OUT OF U.S.</TITLE>\n<DATELINE>    LONDON, April 9 - </DATELINE><BODY>Moderately active grain fixing was\nreported out of the U.S. But none of the business involved the\nsignificant voyages to the Continent or Japan, ship brokers\nsaid.\n    A steady 13.50 dlrs was paid from the U.S. Gulf to Morocco\nand 23.25 dlrs was paid for 27,000 long tons from the Gulf to\nTaiwan. A vessel carrying 13,500 long tons of bagged wheat\nflour from the Gulf to Aqaba received a lump sum of 472,500\ndlrs.\n    Grain from the Great Lakes to Algeria made 28 dlrs against\n27.75 paid for similar fixing towards the end of March.\n    Market talk suggested a Federal Commerce vessel had been\nbooked to move grain from the Great Lakes to Morocco on Comanav\naccount at about 22 dlrs and 15.50 had been paid for a cargo of\noilseeds from British Columbia to Japan, but no confirmation\nwas obtainable.\n    On the Continent, shippers agreed 19 dlrs for wheat from La\nPallice to Buenaventura and 10.75 dlrs for grain from Ghent to\nNaples/Venice range. Elsewhere, maize from East London to Japan\npaid 22 dlrs.\n    Soviet charterers reappeared in the timecharter sector and\nsecured a 30,000 tonner from Savona for a trans-Atlantic round\ntrip at 4,450 dlrs daily and a 31,000 tonner from\nAntwerp-Hamburg for a similar voyage at 4,250 dlrs daily.\n Reuter\n&#3;</BODY></TEXT>\n</REUTERS>")
  }

  test("Bigrams.getBigramsFromFile") {
    val file: File = new File("src/test/resources/mini.sgm")
    val map: BigramsMap = Bigrams.getBigramsFrom(file)
    assert(containsBigram(map, ("moderately", "active"), 1))
    assert(containsBigram(map, ("reported", "out"), 1))
    assert(containsBigram(map, ("active", "grain"), 1))
    assert(containsBigram(map, ("dlrs", "daily"), 2))
    assert(map.get("a").get.size == 9)
  }

  def containsBigram(map: BigramsMap, bigram: (String, String), freq: Int): Boolean = {
    map.get(bigram._1).get.contains(bigram._2) && map.get(bigram._1).get(bigram._2) == freq // TODO: Clean up and enable map("key") call
  }

  test("Bigrams.getBigrams") {
    val tokens = List("hi", "there", "friend", "of", "mine")
    val bigrams: List[(String, String)] = Bigrams.getBigrams(tokens)
    assert(bigrams(0) == ("hi", "there"))
    assert(bigrams(1) == ("there", "friend"))
    assert(bigrams(2) == ("friend", "of"))
    assert(bigrams(3) == ("of", "mine"))
  }

  test("Bigrams.extractBody") {
    val body: String = Bigrams.extractBody("<REUTERS TOPICS=\"YES\" LEWISSPLIT=\"TEST\" CGISPLIT=\"TRAINING-SET\" OLDID=\"5429\" NEWID=\"15531\">\n<DATE> 9-APR-1987 09:40:15.27</DATE>\n<TOPICS><D>grain</D><D>ship</D></TOPICS>\n<PLACES><D>uk</D></PLACES>\n<PEOPLE></PEOPLE>\n<ORGS></ORGS>\n<EXCHANGES></EXCHANGES>\n<COMPANIES></COMPANIES>\n<UNKNOWN>\n\n&#5;&#5;&#5;C G\n&#22;&#22;&#1;f0885&#31;reute\nu f BC-LONDON-FREIGHT-MARKET   04-09 0100</UNKNOWN>\n<TEXT>&#2;\n<TITLE>LONDON FREIGHT MARKET FEATURES GRAIN OUT OF U.S.</TITLE>\n<DATELINE>    LONDON, April 9 - </DATELINE><BODY>Moderately active grain fixing was\nreported out of the U.S. But none of the business involved the\nsignificant voyages to the Continent or Japan, ship brokers\nsaid.\n    A steady 13.50 dlrs was paid from the U.S. Gulf to Morocco\nand 23.25 dlrs was paid for 27,000 long tons from the Gulf to\nTaiwan. A vessel carrying 13,500 long tons of bagged wheat\nflour from the Gulf to Aqaba received a lump sum of 472,500\ndlrs.\n    Grain from the Great Lakes to Algeria made 28 dlrs against\n27.75 paid for similar fixing towards the end of March.\n    Market talk suggested a Federal Commerce vessel had been\nbooked to move grain from the Great Lakes to Morocco on Comanav\naccount at about 22 dlrs and 15.50 had been paid for a cargo of\noilseeds from British Columbia to Japan, but no confirmation\nwas obtainable.\n    On the Continent, shippers agreed 19 dlrs for wheat from La\nPallice to Buenaventura and 10.75 dlrs for grain from Ghent to\nNaples/Venice range. Elsewhere, maize from East London to Japan\npaid 22 dlrs.\n    Soviet charterers reappeared in the timecharter sector and\nsecured a 30,000 tonner from Savona for a trans-Atlantic round\ntrip at 4,450 dlrs daily and a 31,000 tonner from\nAntwerp-Hamburg for a similar voyage at 4,250 dlrs daily.\n Reuter\n&#3;</BODY></TEXT>\n</REUTERS>")
    assert(body == "Moderately active grain fixing was\nreported out of the U.S. But none of the business involved the\nsignificant voyages to the Continent or Japan, ship brokers\nsaid.\n    A steady 13.50 dlrs was paid from the U.S. Gulf to Morocco\nand 23.25 dlrs was paid for 27,000 long tons from the Gulf to\nTaiwan. A vessel carrying 13,500 long tons of bagged wheat\nflour from the Gulf to Aqaba received a lump sum of 472,500\ndlrs.\n    Grain from the Great Lakes to Algeria made 28 dlrs against\n27.75 paid for similar fixing towards the end of March.\n    Market talk suggested a Federal Commerce vessel had been\nbooked to move grain from the Great Lakes to Morocco on Comanav\naccount at about 22 dlrs and 15.50 had been paid for a cargo of\noilseeds from British Columbia to Japan, but no confirmation\nwas obtainable.\n    On the Continent, shippers agreed 19 dlrs for wheat from La\nPallice to Buenaventura and 10.75 dlrs for grain from Ghent to\nNaples/Venice range. Elsewhere, maize from East London to Japan\npaid 22 dlrs.\n    Soviet charterers reappeared in the timecharter sector and\nsecured a 30,000 tonner from Savona for a trans-Atlantic round\ntrip at 4,450 dlrs daily and a 31,000 tonner from\nAntwerp-Hamburg for a similar voyage at 4,250 dlrs daily.\n Reuter\n&#3;")
  }

  test("Bigrams.getSentenceTokens") {
    val text: String = "Here are a few sentences. Here is the second sentence in the few sentences. Here is a third sentence."
    val sentenceTokens: List[List[String]] = Bigrams.getSentenceTokens(text)
    assert(sentenceTokens.head.reduce((s: String, b: String) => s + " " + b) == "here are a few sentences")
    assert(sentenceTokens.tail.head.reduce((s: String, b: String) => s + " " + b) == "here is the second sentence in the few sentences")
    assert(sentenceTokens.tail.tail.head.reduce((s: String, b: String) => s + " " + b) == "here is a third sentence")
  }
}
