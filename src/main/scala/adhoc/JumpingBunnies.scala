package adhoc

object JumpingBunnies {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  @tailrec
  private def calcGCD(a: Long, b: Long): Long = if (b == 0) a else calcGCD(b, a % b)

  private def calcLCM(a: Long, b: Long): Long = {
    val gcd: Long = calcGCD(a, b)
    (a / gcd) * b
  }

  def calcLeastCommonMultiple(jumps: List[Int]): Long = jumps.foldLeft(1L)((acc, jump) => calcLCM(acc, jump))

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val jumps: List[Int] = convertToIntList(reader.next())
    val result: Long = calcLeastCommonMultiple(jumps)
    println(result)
  }
}
