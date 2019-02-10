package memoization

object DifferentWays {
  import scala.collection.mutable.{Map => MutableMap}

  final case class CombinationParameters(n: Int, k: Int)

  private val Modulus: Int = 1e8.toInt + 7

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def calcPascalTriangle(upperLimit: Int): MutableMap[CombinationParameters, Int] = {
    val triangle: MutableMap[CombinationParameters, Int] = MutableMap(CombinationParameters(1, 0) -> 1)
    for {
      n <- 2 to upperLimit
      k <- 0 to math.floor(n / 2).toInt
    } {
      val nChooseK: Int =
        if (k == 0) 1
        else {
          val a: Int = lookup(CombinationParameters(n - 1, k - 1), triangle)
          val b: Int = lookup(CombinationParameters(n - 1, k), triangle)
          (a + b) % Modulus
        }
      triangle += CombinationParameters(n, k) -> nChooseK
    }
    triangle
  }

  private def lookup(p: CombinationParameters, triangle: MutableMap[CombinationParameters, Int]): Int = {
    val CombinationParameters(n, k) = p
    val smallerK: Int = math.min(k, n - k)
    triangle(p.copy(k = smallerK))
  }

  def differentWays(parameters: List[CombinationParameters]): List[Int] = {
    val CombinationParameters(largestN, _) = parameters.maxBy { case CombinationParameters(n, _) => n }
    val combinations: MutableMap[CombinationParameters, Int] = calcPascalTriangle(largestN)
    parameters.map(lookup(_, combinations))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrTestCases: Int = reader.next().toInt
    val parameters: List[CombinationParameters] = (for { _ <- 0 until nrTestCases } yield {
      val List(n, k): List[Int] = convertToIntList(reader.next())
      CombinationParameters(n, k)
      }).toList
    val result: List[Int] = differentWays(parameters)
    result.foreach(println)
  }
}
