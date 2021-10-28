package adhoc

object KunduAndBubbleWrap {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(n, m) => (n, m)
    case _ => throw new Exception("Unexpected input data format.")
  }

  def calculateExpectedNumberOfTrials(n: Int): Double = {
    @tailrec
    def loop(acc: Double, k: Int): Double = {
      if (k == 0) acc
      else loop(acc + n.toDouble / (n - k), k - 1)
    }

    loop(1.0, n - 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (n, m): (Int, Int) = readParameters(reader.next())
    val result: Double = calculateExpectedNumberOfTrials(n * m)
    println(result)
  }
}
