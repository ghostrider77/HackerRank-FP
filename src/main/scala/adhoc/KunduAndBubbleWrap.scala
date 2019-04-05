package adhoc

object KunduAndBubbleWrap {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

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
    val List(n, m): List[Int] = convertToIntList(reader.next())
    val result: Double = calculateExpectedNumberOfTrials(n * m)
    println(result)
  }
}
