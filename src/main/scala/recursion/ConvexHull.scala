package recursion

object ConvexHull {
  import scala.annotation.tailrec
  type Index = Int

  final case class Point(x: Double, y: Double)

  private def convertToDoubleList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  private def findIndexOfPointWithSmallestYCoordinate(points: List[Point]): Index = {
    def isFirstPointSmaller(p: Point, q: Point): Boolean = (p.y < q.y) || (p.y == q.y && p.x < q.x)

    def traverse(acc: (Point, Index), elem: (Point, Index)): (Point, Index) = {
      val (smallest, _): (Point, Index) = acc
      val (p, _): (Point, Index) = elem
      if (isFirstPointSmaller(p, smallest)) elem else acc
    }

    points.zipWithIndex.reduceLeft(traverse)._2
  }

  private def orientation(p1: Point, p2: Point, p3: Point): Double =
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)

  private def sortPointsAngleWithP(p: Point, points: List[Point]): List[Point] = {
    def smallerThanP(q: Point, r: Point): Boolean = {
      val orient: Double = orientation(p, q, r)
      if (orient > 0) true
      else if (orient < 0) false
      else distance(p, q) < distance(p, r)
    }

    points.sortWith(smallerThanP)
  }

  private def keepFarthestPointWithSameAngle(p: Point, sortedPoints: List[Point]): List[Point] = {
    @tailrec
    def loop(acc: List[Point], currentFarthest: Point, xs: List[(Point, Point)]): List[Point] = xs match {
      case Nil => (currentFarthest :: acc).reverse
      case (q, r) :: xss =>
        val orient: Double = orientation(p, q, r)
        if (orient == 0) loop(acc, r, xss)
        else loop(currentFarthest :: acc, r, xss)
      }

    loop(Nil, sortedPoints.head, sortedPoints.sliding(2, 1).collect{ case List(a, b) => (a, b) }.toList)
  }

  private def getReorderedPoints(points: List[Point], ix: Index): (Point, List[Point]) = {
    val (left, right): (List[Point], List[Point]) = points.splitAt(ix)
    val p: Point = right.head
    val sortedPoints: List[Point] = sortPointsAngleWithP(p, left ::: right.tail)
    val reducedSortedPoints: List[Point] = keepFarthestPointWithSameAngle(p, sortedPoints)
    (p, reducedSortedPoints)
  }

  private def runGrahamScan(orderedPoints: List[Point]): List[Point] = {
    val (left, right): (List[Point], List[Point]) = orderedPoints.splitAt(3)

    @tailrec
    def removeConcavePoints(convexHull: List[Point], nextPoint: Point): List[Point] = {
      convexHull.take(2) match {
        case List(q, p) =>
          val orient: Double = orientation(p, q, nextPoint)
          if (orient < 0) removeConcavePoints(convexHull.tail, nextPoint)
          else nextPoint :: convexHull
        case _ => throw new Exception("Not enough point to remove.")
      }
    }

    @tailrec
    def loop(convexHull: List[Point], remainingPoints: List[Point]): List[Point] = remainingPoints match {
      case Nil => convexHull
      case x :: xss =>
        val modifiedHull: List[Point] = removeConcavePoints(convexHull, x)
        loop(modifiedHull, xss)
      }

    loop(left.reverse, right)
  }

  private def distance(p: Point, q: Point): Double = math.hypot(p.x - q.x, p.y - q.y)

  def calcConvexHullPerimeter(points: List[Point]): Double = {
    val ix: Index = findIndexOfPointWithSmallestYCoordinate(points)
    val (startPoint, orderedPoints): (Point, List[Point]) = getReorderedPoints(points, ix)
    val convexHull: List[Point] = runGrahamScan(startPoint :: orderedPoints)
    (startPoint :: convexHull)
      .sliding(2, 1)
      .collect{ case List(p, q) => (p, q) }
      .foldLeft(0.0){ case (perimeter, (p, q)) => perimeter + distance(p, q) }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPoints: Int = reader.next().toInt
    val points: List[Point] =
      reader.take(nrPoints).map(convertToDoubleList).collect{ case List(x, y) => Point(x, y) }.toList
    println(calcConvexHullPerimeter(points))
  }
}
