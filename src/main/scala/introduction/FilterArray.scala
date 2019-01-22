package introduction

object FilterArray {
  import scala.annotation.tailrec

  private def readList(reader: Iterator[String]): List[Int] = {
    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.hasNext) loop(reader.next().toInt :: acc)
      else acc.reverse
    }
    loop(Nil)
  }

  def filter(n: Int, lst: List[Int]): List[Int] = {
    @tailrec
    def loop(acc: List[Int], xs: List[Int]): List[Int] = xs match {
      case Nil => acc.reverse
      case x :: xss => if (x < n) loop(x :: acc, xss) else loop(acc, xss)
    }
    loop(Nil, lst)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val lst: List[Int] = readList(reader)
    val result: List[Int] = filter(n, lst)
    result.foreach(println)
  }
}
