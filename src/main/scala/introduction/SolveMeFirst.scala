package introduction

object SolveMeFirst {
  def sumTwoInts(x: Int, y: Int): Int = x + y

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val a: Int = reader.next().toInt
    val b: Int = reader.next().toInt
    val result: Int = sumTwoInts(a, b)
    println(result)
  }
}
