package introduction

object FilterPositionsInList {
  import scala.annotation.tailrec

  private def readList(reader: Iterator[String]): List[Int] = {
    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.hasNext) loop(reader.next().toInt :: acc)
      else acc.reverse
    }
    loop(Nil)
  }

  def filterOddPositions(lst: List[Int]): List[Int] = lst.zipWithIndex.filter{ case (_, ix) => ix % 2 == 1 }.unzip._1

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val lst: List[Int] = readList(reader)
    val result: List[Int] = filterOddPositions(lst)
    result.foreach(println)
  }
}
