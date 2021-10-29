package introduction

object PolygonArea {
  final case class Point(x: Int, y: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcPolygonArea(points: List[Point]): Double = {
    val signedDoubleArea: Double =
      (points.last :: points)
        .sliding(2)
        .collect{ case List(p, q) => (p, q) }
        .foldLeft(0.0) {
      case (acc, (Point(x1, y1), Point(x2, y2))) => acc + (x1 * y2 - y1 * x2)
    }
    math.abs(signedDoubleArea) / 2
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPoints: Int = reader.next().toInt
    val points: List[Point] =
      reader.take(nrPoints).map(convertToIntList).collect{ case List(x, y) => Point(x, y) }.toList
    val result: Double = calcPolygonArea(points)
    println(result)
  }
}
