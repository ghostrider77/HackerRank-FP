package introduction

object HelloWorldNTimes {
  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    (0 until n).foreach(_ => println("Hello World"))
  }
}
