package recursion

object ConcavePolygon {
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

  private def getReorderedPoints(points: List[Point], ix: Index): (Point, List[Point]) = {
    val (left, right): (List[Point], List[Point]) = points.splitAt(ix)
    val p: Point = right.head
    val sortedPoints: List[Point] = sortPointsAngleWithP(p, left ::: right.tail)
    (p, sortedPoints)
  }

  private def isConcaveTripleFoundDuringGrahamScan(orderedPoints: List[Point]): Boolean = {
    val (left, right): (List[Point], List[Point]) = orderedPoints.splitAt(3)

    @tailrec
    def loop(convexHull: List[Point], remainingPoints: List[Point]): Boolean = remainingPoints match {
      case Nil => false
      case nextPoint :: rest =>
        convexHull.take(2) match {
          case List(q, p) =>
            val orient: Double = orientation(p, q, nextPoint)
            if (orient < 0) true
            else loop(nextPoint :: convexHull, rest)
          case _ => throw new Exception("Not enough points to remove.")
        }
      }

    loop(left.reverse, right)
  }

  private def distance(p: Point, q: Point): Double = math.hypot(p.x - q.x, p.y - q.y)

  def isPolygonConcave(points: List[Point]): Boolean = {
    val ix: Index = findIndexOfPointWithSmallestYCoordinate(points)
    val (p, orderedPoints): (Point, List[Point]) = getReorderedPoints(points, ix)
    isConcaveTripleFoundDuringGrahamScan(p :: orderedPoints)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPoints: Int = reader.next().toInt
    val points: List[Point] =
      reader.take(nrPoints).map(convertToDoubleList).collect{ case List(x, y) => Point(x, y) }.toList
    val result: Boolean = isPolygonConcave(points)
    println(if (result) "YES" else "NO")
  }
}
