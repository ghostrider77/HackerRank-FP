package introduction

object ArrayOfNElements {
  def createList(n: Int): List[Int] = (0 until n).toList

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val result: List[Int] = createList(n)
    println(result.mkString("[", ", ", "]"))
  }
}
