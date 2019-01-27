package recursion

object StringCompression {
  import scala.annotation.tailrec

  def compress(msg: String): String = {
    @tailrec
    def loop(acc: List[(Char, Int)], xs: List[Char]): List[(Char, Int)] = xs match {
      case Nil => acc.reverse
      case x :: _ =>
        val (equal, rest): (List[Char], List[Char]) = xs.span(_ == x)
        loop((x, equal.length) :: acc, rest)
    }

    val compressed: List[(Char, Int)] = loop(Nil, msg.toList)
    compressed
      .flatMap{ case (char, count) => if (count == 1) List(char.toString) else List(char.toString, count.toString) }
      .mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val msg: String = reader.next()
    val result: String = compress(msg)
    println(result)
  }
}
