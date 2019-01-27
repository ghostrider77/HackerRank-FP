package recursion

object SuperQueens {
  final case class Queen(row: Int, column: Int)

  private def doesNotCheckOthers(queen: Queen, queens: List[Queen]): Boolean = {
    val Queen(row, column) = queen
    queens.forall {
      case Queen(r, c) => c != column && math.abs(column - c) != row - r && !inJumpingDistance(r, c, row, column)
    }
  }

  private def inJumpingDistance(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    val xDiff: Int = math.abs(x1 - x2)
    val yDiff: Int = math.abs(y1 - y2)
    (xDiff == 1 && yDiff == 2) || (xDiff == 2 && yDiff == 1)
  }

  def calcNumberOfWays(chessboardSize: Int): Int = {
    def placeQueenIntoRow(k: Int): Set[List[Queen]] = {
      if (k == 0) Set(Nil)
      else {
        for {
          queens <- placeQueenIntoRow(k - 1)
          column <- 1 to chessboardSize
          queen = Queen(k, column)
          if doesNotCheckOthers(queen, queens)
        } yield queen :: queens
      }
    }

    placeQueenIntoRow(chessboardSize).size
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val result: Int = calcNumberOfWays(n)
    println(result)
  }
}
