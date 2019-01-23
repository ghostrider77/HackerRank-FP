package introduction

object FunctionOrNot {
  final case class Pair(x: Int, y: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def isFunction(pairs: List[Pair]): Boolean =
    pairs.groupBy(_.x).mapValues(_.map(_.y).toSet).forall{ case (_, yValues) => yValues.size == 1 }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrTestCases: Int = reader.next().toInt
    val testCases: List[List[Pair]] = (for { _ <- 0 until nrTestCases } yield {
      val n: Int = reader.next().toInt
      val pairs: List[Pair] = (for { _ <- 0 until n } yield {
        val List(x, y): List[Int] = convertToIntList(reader.next())
        Pair(x, y)
      }).toList
      pairs
    }).toList
    val result: List[Boolean] = testCases.map(isFunction)
    result.foreach(verdict => println(if (verdict) "YES" else "NO"))
  }
}
