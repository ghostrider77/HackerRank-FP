package memoization

object PentagonalNumbers {
  def pentagonalNumber(n: Long): Long = n * (3*n - 1) / 2

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val lst: List[Long] = (for (_ <- 0 until numberOfTestCases) yield reader.next().toLong).toList
    val result: List[Long] = lst.map(pentagonalNumber)
    result.foreach(println)
  }
}
