package adhoc

object GameOfKayles {
  import scala.annotation.tailrec
  private val MaximumNumberOfPins: Int = 300

  private def minimumExcludedValue(numbers: Set[Int]): Int = {
    @tailrec
    def loop(mex: Int): Int = {
      if (numbers.contains(mex)) loop(mex + 1)
      else mex
    }
    loop(0)
  }

  private def calcGrundyNumbers(n: Int): Vector[Int] = {
    val grundyNumbers: Array[Int] = Array.fill(n + 1)(0)

    def calcGrundy(k: Int): Int = {
      if (k <= 2) k
      else {
        val nimSums: Set[Int] = (for {
          nrPinsRemoved <- 1 to 2
          size <- 0 to (k - nrPinsRemoved) / 2
        } yield grundyNumbers(size) ^ grundyNumbers(k - size - nrPinsRemoved)).toSet
        minimumExcludedValue(nimSums)
      }
    }

    for { k <- 0 to n } grundyNumbers(k) = calcGrundy(k)
    grundyNumbers.toVector
  }

  def doesFirstPlayerWin(games: List[String]): List[Boolean] = {
    val grundyNumbers: Vector[Int] = calcGrundyNumbers(MaximumNumberOfPins)
    for { game <- games } yield {
      val pinConfiguration: List[Int] = game.split("X").map(_.length).toList
      pinConfiguration.foldLeft(0)((acc, groupSize) => acc ^ grundyNumbers(groupSize)) != 0
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val configurations: List[String] = (for { _ <- 0 until numberOfTestCases }
      yield {
        val _: Int = reader.next().toInt
        reader.next()
      }).toList
    val results: List[Boolean] = doesFirstPlayerWin(configurations)
    results.foreach(if (_) println("WIN") else println("LOSE"))
  }
}
