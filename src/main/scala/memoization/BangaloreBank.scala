package memoization

object BangaloreBank {
  import scala.annotation.tailrec

  final case class FingerPositions(left: Int, right: Int, time: Int)

  def calcMinimumTypingTime(accountNumber: List[Int]): Int = {
    @tailrec
    def loop(currentPositions: List[FingerPositions], xs: List[Int]): Int = xs match {
      case Nil => currentPositions.minBy(_.time).time
      case x :: xss =>
        val possibleMoves: List[FingerPositions] = currentPositions.flatMap{
          case FingerPositions(currentLeft, currentRight, currentMinTime) =>
            List(
              FingerPositions(left = x, right = currentRight, time = currentMinTime + math.abs(x - currentLeft) + 1),
              FingerPositions(left = currentLeft, right = x, time = currentMinTime + math.abs(x - currentRight) + 1)
            )
        }
        val updatedPositions: List[FingerPositions] = possibleMoves
          .groupBy{ case FingerPositions(left, right, _) => (left, right) }
          .mapValues(_.minBy(_.time))
          .values
          .toList
        loop(updatedPositions, xss)
    }

    val initialTypingTimes: List[FingerPositions] = (for {
      left <- 1 to 10
      right <- 1 to 10
    } yield FingerPositions(left, right, 0)).toList
    loop(initialTypingTimes, accountNumber)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val accountNumber: List[Int] = reader.next().split(" ").map(digit => if (digit == "0") 10 else digit.toInt).toList
    val result: Int = calcMinimumTypingTime(accountNumber)
    println(result)
  }
}
