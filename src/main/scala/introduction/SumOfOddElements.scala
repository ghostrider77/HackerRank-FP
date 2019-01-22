package introduction

object SumOfOddElements {
  import scala.annotation.tailrec

  private def readList(reader: Iterator[String]): List[Int] = {
    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.hasNext) loop(reader.next().toInt :: acc)
      else acc.reverse
    }
    loop(Nil)
  }

  def calcSumOfOddElems(lst: List[Int]): Int = lst.filter(_ % 2 != 0).sum

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val lst: List[Int] = readList(reader)
    val result: Int = calcSumOfOddElems(lst)
    println(result)
  }
}
