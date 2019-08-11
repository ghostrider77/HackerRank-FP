package recursion

object Crosswords101 {
  import scala.language.implicitConversions

  sealed trait Direction
  case object Horizontal extends Direction
  case object Vertical extends Direction

  private val boardSize: Int = 10
  private val FreeCell: Char = '-'

  final case class Cell(i: Int, j: Int)

  final case class Board(board: Map[Cell, Char]) {
    override def toString: String = {
      (0 until boardSize).map(
        ix => (0 until boardSize).map(jy => board(Cell(ix, jy))).mkString
      ).mkString("\n")
    }

    def apply(cell: Cell): Char = board(cell)
  }

  implicit private def toBoard(board: Map[Cell, Char]): Board = Board(board)

  def readBoard(reader: Iterator[String]): Board =
    (for {
      ix <- 0 until boardSize
      line: String = reader.next()
      (char, jy) <- line.zipWithIndex
    } yield Cell(ix, jy) -> char).toMap

  private def hasEmptyField(board: Board): Boolean = board.board.valuesIterator.contains(FreeCell)

  private def doesWordFitOnBoard(startCell: Cell, direction: Direction, word: String): Boolean = {
    val Cell(ix, jy) = startCell
    val startIndex: Int = direction match {
      case Horizontal => jy
      case Vertical => ix
    }
    startIndex + word.length <= boardSize
  }

  private def validWordInsertion(board: Board, cells: List[Cell], word: String): Boolean = {
    def isConsistent(cell: Cell, char: Char): Boolean = {
      val characterAtCell: Char = board(cell)
      characterAtCell == FreeCell || characterAtCell == char
    }
    cells.view.zip(word).forall{ case (cell, char) => isConsistent(cell, char) }
  }

  private def addWordToBoard(board: Board, cells: List[Cell], word: String): Board = {
    val updated: Map[Cell, Char] = cells.view.zip(word).map{ case (cell, char) => cell -> char }.toMap
    board.board ++ updated
  }

  private def placeWord(board: Board, startCell: Cell, direction: Direction, word: String): Option[Board] = {
    if (!doesWordFitOnBoard(startCell, direction, word)) None
    else {
      val Cell(ix, jy) = startCell
      val cells: List[Cell] = (direction match {
        case Horizontal => (0 until word.length).map(k => Cell(ix, jy + k))
        case Vertical => (0 until word.length).map(k => Cell(ix + k, jy))
      }).toList
      if (!validWordInsertion(board, cells, word)) None
      else Some(addWordToBoard(board, cells, word))
    }
  }

  def solveCrossword(board: Board, words: List[String]): List[Board] = words match {
    case Nil => if (hasEmptyField(board)) Nil else List(board)
    case w :: wss =>
      val possibleUpdatedBoards: List[Board] = for {
        direction <- List(Horizontal, Vertical)
        ix <- 0 until boardSize
        jy <- 0 until boardSize
        updatedBoard <- placeWord(board, Cell(ix, jy), direction, w)
      } yield updatedBoard
      possibleUpdatedBoards.flatMap(solveCrossword(_, wss))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val board: Board = readBoard(reader)
    val words: List[String] = reader.next().split(";").toList
    val result: List[Board] = solveCrossword(board, words)
    println(result.head)
  }
}
