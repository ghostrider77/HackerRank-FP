package adhoc

object Puzzle {
  final case class Point(x: Int, y: Int)

  final case class Tromino(c1: Point, c2: Point, c3: Point) {
    override def toString: String = List(c1.x, c1.y, c2.x, c2.y, c3.x, c3.y).mkString(" ")
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(row, column) => (row, column)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def exponentiate(base: Int, exponent: Int): Int = (0 until exponent).foldLeft(1)((acc, _) => base * acc)

  private def fillTwoByTwoGrid(coveredPoint: Point, upperLeftCorner: Point): List[Tromino] = {
    val Point(x, y) = upperLeftCorner
    val points: List[Point] = (for {
      dx <- 0 to 1
      dy <- 0 to 1
      p = Point(x + dx, y + dy)
      if p != coveredPoint
    } yield p).toList
    points match {
      case List(p1, p2, p3) => List(Tromino(p1, p2, p3))
      case _ => throw new Exception("There must be 3 suitable points on a 2-by-2 grid.")
    }
  }

  private def pointInQuarterGivenByCorner(corner: Point, p: Point, size: Int): Boolean =
    corner.x <= p.x && corner.x + size > p.x && corner.y <= p.y && corner.y + size > p.y

  private def calcCentralPoints(upperLeftCorner: Point, halfSize: Int, coveredPoint: Point): List[Point] = {
    val Point(cornerX, cornerY) = upperLeftCorner
    (for {
      dx <- 0 to 1
      dy <- 0 to 1
      p = Point(cornerX + halfSize - 1 + dx, cornerY + halfSize - 1 + dy)
      corner = Point(cornerX + dx*halfSize, cornerY + dy*halfSize)
      if !pointInQuarterGivenByCorner(corner, coveredPoint, halfSize)
    } yield p).toList
  }

  def solvePuzzle(size: Int, coveredPoint: Point, upperLeftCorner: Point): List[Tromino] = {
    if (size == 1) Nil
    else if (size == 2) fillTwoByTwoGrid(coveredPoint, upperLeftCorner)
    else {
      val halfSize: Int = size / 2
      val middlePoints @ List(p1, p2, p3): List[Point] = calcCentralPoints(upperLeftCorner, halfSize, coveredPoint)
      val coveredPoints: List[Point] = coveredPoint :: middlePoints
      val newTromino = Tromino(p1, p2, p3)
      newTromino :: (for {
        dx <- 0 to 1
        dy <- 0 to 1
        corner = Point(upperLeftCorner.x + dx*halfSize, upperLeftCorner.y + dy*halfSize)
      } yield {
        val coveredInTheSameSquare: Point = coveredPoints.find(pointInQuarterGivenByCorner(corner, _, halfSize)).get
        solvePuzzle(halfSize, coveredInTheSameSquare, corner)
      }).toList.flatten
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val exponent: Int = reader.next().toInt
    val (row, colum): (Int, Int) = readParameters(reader.next())
    val size: Int = exponentiate(2, exponent)
    val coveredPoint = Point(row, colum)
    val upperLeftCorner = Point(1, 1)
    val result: List[Tromino] = solvePuzzle(size, coveredPoint, upperLeftCorner)
    result.foreach(println)
  }
}
