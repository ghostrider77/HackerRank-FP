package memoization

object Fibonacci {
  private val Modulus: Int = (1e8 + 7).toInt
  private val NMax: Int = 10000

  def calcModuloFibonacci(ns: List[Int]): List[Int] = {
    val table: Array[Int] = Array.fill(NMax + 1)(0)
    table(1) = 1
    for { n <- 2 to NMax } table(n) = (table(n - 1) + table(n - 2)) % Modulus

    ns.map(table(_))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val nValues: List[Int] = (for (_ <- 0 until numberOfTestCases) yield reader.next().toInt).toList
    val result: List[Int] = calcModuloFibonacci(nValues)
    result.foreach(println)
  }
}
