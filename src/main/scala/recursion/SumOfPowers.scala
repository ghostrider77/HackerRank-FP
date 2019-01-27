package recursion

object SumOfPowers {
  private def getSuitableSummands(n: Int, e: Int): List[Int] ={
    val maxBase: Int = math.pow(n, 1 / e.toDouble).floor.toInt
    (maxBase to 1 by -1).map(math.pow(_, e).toInt).toList
  }

  def sumOfPowers(number: Int, exponent: Int): Int = {
    def combinations(number: Int, powers: List[Int]): Int = {
      if (number == 0) 1
      else powers match {
        case Nil => 0
        case p :: pss =>
          if (number >= p) combinations(number - p, pss) + combinations(number, pss)
          else combinations(number, pss)
      }
    }

    val powers: List[Int] = getSuitableSummands(number, exponent)
    combinations(number, powers)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val exponent : Int = reader.next().toInt
    val nrWays: Int = sumOfPowers(n, exponent)
    println(nrWays)
  }
}
