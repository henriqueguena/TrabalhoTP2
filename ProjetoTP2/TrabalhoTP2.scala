import scala.util.matching.Regex
import scala.io.Source
import scala.collection.mutable.Map
import java.text.Normalizer

object WordFrequency {

  val stopWords: Set[String] = Source.fromFile("teste.txt", "UTF-8").getLines.mkString.split(",").toSet

  val wordPattern: Regex = """\p{L}+""".r

  def countWords(words: List[String], wordFreqs: Map[String, Int], chunkSize: Int): Unit = {
    if (words.isEmpty) return

    val chunk = words.take(chunkSize)
    countWords(words.drop(chunkSize), wordFreqs, chunkSize) // Chamada recursiva

    processChunk(chunk, wordFreqs)
  }

  def processChunk(words: List[String], wordFreqs: Map[String, Int]): Unit = {
    for (word <- words) {
      val normalizedWord = normalizeString(word)
      if (!stopWords.contains(normalizedWord)) {
        wordFreqs.get(normalizedWord) match {
          case Some(count) => wordFreqs.update(normalizedWord, count + 1)
          case None => wordFreqs(normalizedWord) = 1
        }
      }
    }
  }

  def printWordFreqs(wordFreqs: List[(String, Int)]): Unit = {
    if (wordFreqs.isEmpty) return
    val (word, count) = wordFreqs.head
    val wordWithoutAccents = removeAccents(word)
    println(wordWithoutAccents + " - " + count)
    printWordFreqs(wordFreqs.tail)
  }

  def normalizeString(str: String): String = {
    str.toLowerCase
  }

  def removeAccents(str: String): String = {
    val normalizedStr = Normalizer.normalize(str, Normalizer.Form.NFD)
    normalizedStr.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: word_frequency <filename>")
      sys.exit(1)
    }

    val words = wordPattern.findAllIn(Source.fromFile(args(0), "UTF-8").mkString.toLowerCase).toList

    val wordFreqs: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()

    countWords(words, wordFreqs, 100) // Tamanho do chunk

    printWordFreqs(wordFreqs.toList.sortBy { case (_, count) => count }.reverse.take(25))
  }
}

