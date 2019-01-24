package introduction

object PolygonPerimeter {
  final case class Point(x: Int, y: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def segmentLength(p: Point, q: Point): Double = math.hypot(p.x - q.x, p.y - q.y)

  def calcPolygonPerimeter(points: List[Point]): Double =
    (points.last :: points).sliding(2).foldLeft(0.0){ case (acc, List(p, q)) => acc + segmentLength(p, q) }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPoints: Int = reader.next().toInt
    val points: List[Point] = (for { _ <- 0 until nrPoints } yield {
      val List(x, y): List[Int] = convertToIntList(reader.next())
      Point(x, y)
    }).toList
    val result: Double = calcPolygonPerimeter(points)
    println(result)
  }
}
