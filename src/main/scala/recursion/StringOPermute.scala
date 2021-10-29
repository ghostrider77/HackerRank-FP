package recursion

object StringOPermute {
  def permute(s: String): String = s.toList.grouped(2).collect{ case List(c1, c2) => List(c2, c1) }.flatten.mkString

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val strings: List[String] = (0 until n).map(_ => reader.next()).toList
    val result: List[String] = strings.map(permute)
    result.foreach(println)
  }
}
