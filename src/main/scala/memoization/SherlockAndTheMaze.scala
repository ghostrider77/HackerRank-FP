package memoization

object SherlockAndTheMaze {
  private val Modulus: Int = (1e9 + 7).toInt

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def checkCornerCases(i: Int, j: Int, turns: Int, direction: Int): Option[Int] = {
    if (i < 0 || j < 0) Some(0)
    else if (i == 0 && j == 0) Some(1)
    else if (turns == -1) {
      if (direction == 0 && i == 0) Some(1)
      else if (direction == 1 && j == 0) Some(1)
      else Some(0)
    }
    else None
  }

  def calcNumberOfPathsWithAtMostKTurns(n: Int, m: Int, k: Int): Int = {
    val dict: Array[Array[Array[Array[Int]]]] = Array.fill[Int](n, m, k, 2)(-1)

    def numberOfPaths(i: Int, j: Int, turns: Int, direction: Int): Int = {
      checkCornerCases(i, j, turns, direction) match {
        case Some(result) => result
        case None =>
          if (dict(i)(j)(turns)(direction) != -1) dict(i)(j)(turns)(direction)
          else {
            val numberOfWays: Int =
              (if (direction == 0) numberOfPaths(i, j - 1, turns, 0) + numberOfPaths(i - 1, j, turns - 1, 1)
              else numberOfPaths(i - 1, j, turns, 1) + numberOfPaths(i, j - 1, turns - 1, 0)) % Modulus
            dict(i)(j)(turns)(direction) = numberOfWays
            numberOfWays
          }
      }
    }

    if (n == 1 && m == 1) 1
    else (numberOfPaths(n - 2, m - 1, k - 1, 1) + numberOfPaths(n - 1, m - 2, k - 1, 0)) % Modulus
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val data: List[List[Int]] = (for { _ <- 0 until numberOfTestCases} yield convertToIntList(reader.next())).toList
    val results: List[Int] = data.collect{ case List(n, m, k) => calcNumberOfPathsWithAtMostKTurns(n, m, k) }
    results.foreach(println)
  }
}
