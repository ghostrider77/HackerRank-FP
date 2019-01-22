package introduction

object ListReplication {
  import scala.annotation.tailrec

  private def readList(reader: Iterator[String]): List[Int] = {
    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.hasNext) loop(reader.next().toInt :: acc)
      else acc.reverse
    }
    loop(Nil)
  }

  def replicate(n: Int, lst: List[Int]): List[Int] = lst.flatMap(List.fill(n)(_))

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val lst: List[Int] = readList(reader)
    val result: List[Int] = replicate(n, lst)
    result.foreach(println)
  }
}
