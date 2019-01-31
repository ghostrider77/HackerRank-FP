package memoization

object BitterChocolate {
  import scala.collection.mutable.{Map => MutableMap}

  final case class Board(upper: Int, middle: Int, lower: Int) {
    require(upper <= middle && middle <= lower)
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def doesFirstPlayerWin(initialBoard: Board): Boolean = {
    val firstWins: MutableMap[Board, Boolean] = MutableMap(Board(0, 0, 1) -> false)

    def isWinningConfiguration(board: Board): Boolean = {
      def firstCanWin: Boolean = {
        val Board(a, b, c) = board
        val movingToUpperRow: List[Board] = (for { i <- a - 1 to 0 by -1 } yield Board(i, b, c)).toList
        val movingToMiddleRow: List[Board] = (for { j <- b - 1 to 0 by -1 } yield Board(a min j, j, c)).toList
        val movingToLowerBoard: List[Board] = (for { k <- c - 1 to 1 by -1 } yield Board (a min k, b min k, k)).toList
        val boardsToMove: List[Board] = movingToUpperRow ::: movingToMiddleRow ::: movingToLowerBoard
        boardsToMove.exists(!isWinningConfiguration(_))
      }

      firstWins.getOrElseUpdate(board, firstCanWin)
    }

    isWinningConfiguration(initialBoard)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val results: List[Boolean] = (for { _ <- 0 until numberOfTestCases } yield {
      val List(lower, middle, upper): List[Int] = convertToIntList(reader.next())
      doesFirstPlayerWin(Board(upper, middle, lower))
    }).toList

    results.foreach(if (_) println("WIN") else println("LOSE"))
  }
}
