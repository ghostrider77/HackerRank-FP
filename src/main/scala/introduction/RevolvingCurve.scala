package introduction

object RevolvingCurve {
  private val h: Double = 0.001

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(n, m) => (n, m)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def evaluate(coefficients: List[Int], exponents: List[Int], x: Double): Double =
    coefficients.zip(exponents).foldLeft(0.0){ case (acc, (a, b)) => acc + a * math.pow(x, b) }

  private def trapezoidalRule(ys: List[Double]): Double = {
    if (ys.length < 2) 0.0
    else (ys.sum - (ys.head + ys.last) / 2) * h
  }

  def calcIntegral(coefficients: List[Int], exponents: List[Int], left: Int, right: Int): Double = {
    val xValues: List[Double] = List.tabulate(1000 * (right - left) + 1)(x => left + x.toDouble * h)
    val yValues: List[Double] = xValues.map(evaluate(coefficients, exponents, _))
    trapezoidalRule(yValues)
  }

  def calcVolume(coefficients: List[Int], exponents: List[Int], left: Int, right: Int): Double = {
    val xValues: List[Double] = List.tabulate(1000 * (right - left) + 1)(x => left + x.toDouble * h)
    val ySquareValues: List[Double] = xValues.map(x => math.pow(evaluate(coefficients, exponents, x), 2))
    math.Pi * trapezoidalRule(ySquareValues)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val coefficients: List[Int] = convertToIntList(reader.next())
    val exponents: List[Int] = convertToIntList(reader.next())
    val (left, right): (Int, Int) = readParameters(reader.next())
    val area: Double = calcIntegral(coefficients, exponents, left, right)
    val volume: Double = calcVolume(coefficients, exponents, left, right)
    println(area)
    println(volume)
  }
}
