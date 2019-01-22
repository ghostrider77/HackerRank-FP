package introduction

object EvaluatingExp {
  private val nrTerms: Int = 10

  def exponential(x: Double): Double =
    (0 until nrTerms).scanLeft(0.0)((acc, k) => if (k == 0) 1.0 else acc * x / k).toList.sum

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrTestCases: Int = reader.next().toInt
    val exponents: List[Double] = (for { _ <- 0 until nrTestCases } yield reader.next().toDouble).toList
    val result: List[Double] = exponents.map(exponential)
    result.foreach(println)
  }
}
