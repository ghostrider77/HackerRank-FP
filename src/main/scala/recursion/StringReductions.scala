package recursion

object StringReductions {
  import scala.annotation.tailrec

  def keepUniqueCharacters(s: String): String = {
    @tailrec
    def loop(acc: List[Char], alreadyEncountered: Set[Char], xs: List[Char]): List[Char] = xs match {
      case Nil => acc.reverse
      case x :: xss =>
        if (alreadyEncountered.contains(x)) loop(acc, alreadyEncountered, xss)
        else loop(x :: acc, alreadyEncountered + x, xss)
    }

    val compressed: List[Char] = loop(Nil, Set(), s.toList)
    compressed.mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val s: String = reader.next()
    val compressed: String = keepUniqueCharacters(s)
    println(compressed)
  }
}
