package adhoc

object CommonDivisors {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  @tailrec
  private def calcGCD(a: Int, b: Int): Int = if (b == 0) a else calcGCD(b, a % b)

  def calcNumberOfCommonDivisors(n: Int, m: Int): Int = {
    val gcd: Int = calcGCD(n, m)
    val upperLimit: Int = math.sqrt(gcd).toInt
    val nrCommonDivisors: Int =
      (1 to upperLimit).foldLeft(0){ case (acc, elem) => if (gcd % elem == 0) acc + 2 else acc }
    if (gcd == upperLimit * upperLimit) nrCommonDivisors - 1
    else nrCommonDivisors
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrCases: Int = reader.next().toInt
    val testCases: List[List[Int]] = (for { _ <- 0 until nrCases } yield convertToIntList(reader.next())).toList
    val results: List[Int] = testCases.map{ case List(n, m) => calcNumberOfCommonDivisors(n, m) }
    results.foreach(println)
  }
}
