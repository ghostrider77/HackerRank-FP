package recursion

object StringMingling {
  import scala.annotation.tailrec

  def interleave(s1: String, s2: String): String = {
    @tailrec
    def loop(acc: List[Char], xs: List[(Char, Char)]): List[Char] = xs match {
      case Nil => acc.reverse
      case (c1, c2) :: xss => loop(c2 :: c1 :: acc, xss)
    }

    loop(Nil, s1.zip(s2).toList).mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val s1: String = reader.next()
    val s2: String = reader.next()
    val result: String = interleave(s1, s2)
    println(result)
  }
}
