package recursion

object SuperDigit {
  import scala.annotation.tailrec

  private def readParameters(line: String): (String, String) = line.split(" ").toList match {
    case List(n, m) => (n, m)
    case _ => throw new Exception("Unexpected input data format.")
  }

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
    val (n, k): (String, String) = readParameters(reader.next())
    val result: Int = calcCompoundSuperDigit(n, k.toInt)
    println(result)
  }
}
