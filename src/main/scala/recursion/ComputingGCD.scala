package recursion

object ComputingGCD {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(n, m) => (n, m)
    case _ => throw new Exception("Unexpected input data format.")
  }

  @tailrec
  def calcGCD(a: Int, b: Int): Int = if (b == 0) a else calcGCD(b, a % b)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (a, b): (Int, Int) = readParameters(reader.next())
    val result: Int = calcGCD(a, b)
    println(result)
  }
}
