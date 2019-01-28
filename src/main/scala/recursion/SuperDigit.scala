package recursion

object SuperDigit {
  import scala.annotation.tailrec

  @tailrec
  private def calcSuperDigit(numberAsString: String): Int = {
    if (numberAsString.length == 1) numberAsString.toInt
    else calcSuperDigit(numberAsString.foldLeft(0)((acc, c) => acc + c.asDigit).toString)
  }

  def calcCompoundSuperDigit(n: String, k: Int): Int = {
    val superDigitOfN: Int = calcSuperDigit(n)
    val concatenatedString: String = (superDigitOfN * k).toString
    val superDigitOfConcatenatedString: Int = calcSuperDigit(concatenatedString)
    superDigitOfConcatenatedString
  }

  def main(args: Array[String]): Unit = {
    val reader = scala.io.Source.stdin.getLines()
    val List(n, k): List[String] = reader.next().split(" ").toList
    val result: Int = calcCompoundSuperDigit(n, k.toInt)
    println(result)
  }
}
