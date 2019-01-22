package introduction

object UpdateList {
  import scala.annotation.tailrec

  private def readList(reader: Iterator[String]): List[Int] = {
    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.hasNext) loop(reader.next().toInt :: acc)
      else acc.reverse
    }
    loop(Nil)
  }

  def replaceWithAbsoluteValue(lst: List[Int]): List[Int] = lst.map(_.abs)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val lst: List[Int] = readList(reader)
    val result: List[Int] = replaceWithAbsoluteValue(lst)
    result.foreach(println)
  }
}
