package introduction

object ReverseAList {
  import scala.annotation.tailrec

  private def readList(reader: Iterator[String]): List[Int] = {
    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.hasNext) loop(reader.next().toInt :: acc)
      else acc.reverse
    }
    loop(Nil)
  }

  def reverse(lst: List[Int]): List[Int] = {
    @tailrec
    def loop(acc: List[Int], xs: List[Int]): List[Int] = xs match {
      case Nil => acc
      case x :: xss => loop(x :: acc, xss)
    }
    loop(Nil, lst)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val lst: List[Int] = readList(reader)
    val result: List[Int] = reverse(lst)
    result.foreach(println)
  }
}
