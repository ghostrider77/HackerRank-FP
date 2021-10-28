package memoization

object DicePath {
  import scala.collection.mutable.{Map => MutableMap}
  type MaxPathConfigs = Map[Dice, Int]

  sealed trait Direction
  final case object Down extends Direction
  final case object Right extends Direction

  final case class Dice(top: Int, right: Int, front: Int)
  final case class Point(i: Int, j: Int)

  private val OppositeSideSum: Int = 7

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def getNewDiceConfiguration(current: Dice, direction: Direction): Dice = direction match {
    case Down => Dice(top = OppositeSideSum - current.front, right = current.right, front = current.top)
    case Right => Dice(top = OppositeSideSum - current.right, right = current.top, front = current.front)
  }

  private def processPoint(point: MaxPathConfigs, direction: Direction): MaxPathConfigs =
    for {
      (dice, maxValue) <- point
      rolledDice = getNewDiceConfiguration(dice, direction)
    } yield rolledDice -> (maxValue + rolledDice.top)


  private def processLeftAndUpCandidates(pointUp: MaxPathConfigs, pointLeft: MaxPathConfigs): MaxPathConfigs = {
    val fromUp: MaxPathConfigs = processPoint(pointUp, direction = Down)
    val fromLeft: MaxPathConfigs = processPoint(pointLeft, direction = Right)
    (fromUp.toList ::: fromLeft.toList).groupBy{ case (dice, _) => dice }.view.mapValues(_.map(_._2).max).toMap
  }

  private def maximalDicePath(nRows: Int, nCols: Int, grid: MutableMap[Point, MaxPathConfigs]): Int = {

    def fillGrid(n: Int, m: Int): MaxPathConfigs = {
      for { i <- 1 until n } {
        if (!grid.contains(Point(i, 0))) {
          val pointUp: MaxPathConfigs = grid(Point(i - 1, 0))
          val (singleUpperDice, maxValue): (Dice, Int) = pointUp.head
          val newConfiguration: Dice = getNewDiceConfiguration(singleUpperDice, Down)
          grid(Point(i, 0)) = Map(newConfiguration -> (maxValue + newConfiguration.top))
        }
      }

      for { j <- 1 until m } {
        if (!grid.contains(Point(0, j))) {
          val pointLeft: MaxPathConfigs = grid(Point(0, j - 1))
          val (singleLeftDice, maxValue): (Dice, Int) = pointLeft.head
          val newConfiguration: Dice = getNewDiceConfiguration(singleLeftDice, Right)
          grid(Point(0, j)) = Map(newConfiguration -> (maxValue + newConfiguration.top))
        }
      }

      for {
        i <- 1 until n
        j <- 1 until m
      } {
        if (!grid.contains(Point(i, j))) {
          val pointUp: MaxPathConfigs = grid(Point(i - 1, j))
          val pointLeft: MaxPathConfigs = grid(Point(i, j - 1))
          grid(Point(i, j)) = processLeftAndUpCandidates(pointUp, pointLeft)
        }
      }
      grid(Point(n - 1, m - 1))
    }

    val bottomRightDiceConfigs: MaxPathConfigs =
      grid.getOrElseUpdate(Point(nRows - 1, nCols - 1), fillGrid(nRows, nCols))
    bottomRightDiceConfigs.valuesIterator.max
  }

  def calcSumOfMaximalPaths(gridsizes: List[(Int, Int)]): List[Int] = {
    val grid: MutableMap[Point, MaxPathConfigs] = MutableMap()
    val initialDiceConfiguration: Dice = Dice(top = 1, right = 4, front = 2)
    grid(Point(0, 0)) = Map(initialDiceConfiguration -> 1)
    gridsizes.map{ case (n, m) => maximalDicePath(n, m, grid) }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestcases: Int = reader.next().toInt
    val gridSizes: List[(Int, Int)] = (for { _ <- 0 until numberOfTestcases } yield {
      val List(n, m): List[Int] = convertToIntList(reader.next())
      (n, m)
    }).toList
    val result: List[Int] = calcSumOfMaximalPaths(gridSizes)
    result.foreach(println)
  }
}
