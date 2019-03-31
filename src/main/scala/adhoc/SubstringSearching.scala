package adhoc

object SubstringSearching {
  import scala.annotation.tailrec

  private val Separator: String = "$"
  final case class TextData(text: String, pattern: String)

  private def calcPrefixFunction(string: String): List[Int] = {
    val prefixArray: Array[Int] = Array.fill(string.length)(0)

    @tailrec
    def updateBorder(border: Int, letter: Char): Int = {
      if (border > 0 && letter != string(border)) updateBorder(prefixArray(border - 1), letter)
      else border
    }

    @tailrec
    def loop(border: Int, xs: List[(Char, Int)]): Unit = xs match {
      case Nil => ()
      case (letter, ix) :: xss =>
        val updatedBorder1: Int = updateBorder(border, letter)
        val updatedBorder2: Int = if (letter == string(updatedBorder1)) updatedBorder1 + 1 else 0
        prefixArray(ix) = updatedBorder2
        loop(updatedBorder2, xss)
    }

    loop(border = 0, string.toList.zipWithIndex.drop(1))
    prefixArray.toList
  }

  def doesPatternOccurAsSubstring(testCase: TextData): Boolean = {
    val TextData(text, pattern) = testCase
    val patternLength: Int = pattern.length
    val prefixFunction: List[Int] = calcPrefixFunction(pattern + Separator + text)
    prefixFunction.zipWithIndex.exists { case (index, ix) => ix > patternLength && index == patternLength }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrTestCases: Int = reader.next().toInt
    val testCases: List[TextData] = (for { _ <- 0 until nrTestCases } yield {
      val text: String = reader.next()
      val pattern: String = reader.next()
      TextData(text, pattern)
    }).toList
    val result: List[Boolean] = testCases.map(doesPatternOccurAsSubstring)
    result.foreach(verdict => println(if (verdict) "YES" else "NO"))
  }
}
