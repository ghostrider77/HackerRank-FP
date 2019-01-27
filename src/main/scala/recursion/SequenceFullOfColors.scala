package recursion

object SequenceFullOfColors {
  import scala.annotation.tailrec

  def hasFullColors(sequence: String): Boolean = {
    def prefixCondition(colors: Map[Char, Int]): Boolean =
      math.abs(colors('R') - colors('G')) <= 1 && math.abs(colors('B') - colors('Y')) <= 1

    @tailrec
    def loop(acc: Map[Char, Int], colors: List[Char]): Boolean = colors match {
      case Nil => prefixCondition(acc) && acc('R') == acc('G') && acc('B') == acc('Y')
      case c :: css =>
        val updatedColors: Map[Char, Int] = acc.updated(c, acc(c) + 1)
        if (prefixCondition(updatedColors)) loop(updatedColors, css)
        else false
      }

    loop(Map('R' -> 0, 'G' -> 0, 'B' -> 0, 'Y' -> 0), sequence.toList)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrTestCases: Int = reader.next().toInt
    val sequences: List[String] = (for { _ <- 0 until nrTestCases} yield reader.next()).toList
    val result: List[Boolean] = sequences.map(hasFullColors)
    result.foreach(verdict => println(if (verdict) "True" else "False"))
  }
}
