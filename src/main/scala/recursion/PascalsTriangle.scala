package recursion

object PascalsTriangle {
  import scala.annotation.tailrec
  type Row = List[Int]

  def calcPascalsTriangle(n: Int): List[Row] = {
    @tailrec
    def loop(acc: List[Row], level: Int): List[Row] = {
      if (level == n) acc.reverse
      else {
        val nextRow: Row = (for { List(a, b) <- acc.head.sliding(2) } yield a + b).toList
        loop(List(1, 1).patch(1, nextRow, 0) :: acc, level + 1)
      }
    }
    loop(List(List(1)), 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val result: List[Row] = calcPascalsTriangle(n)
    result.foreach(row => println(row.mkString(" ")))
  }
}
