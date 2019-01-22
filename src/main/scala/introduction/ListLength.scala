package introduction

object ListLength {
  import scala.annotation.tailrec

  private def readList(reader: Iterator[String]): List[Int] = {
    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.hasNext) loop(reader.next().toInt :: acc)
      else acc.reverse
    }
    loop(Nil)
  }

  def length(lst: List[Int]): Int = lst.foldLeft(0)((acc, _) => acc + 1)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val lst: List[Int] = readList(reader)
    val result: Int = length(lst)
    println(result)
  }
}
