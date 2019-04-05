package functional_structures

object ListAndGCD {
  type CanonicalForm = Map[Int, Int]

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private[functional_structures] def readCanonicalForm(line: String): CanonicalForm =
    convertToIntList(line).grouped(2).map{ case List(p, exp) => (p, exp) }.toMap

  def calcGCD(numbers: List[CanonicalForm]): CanonicalForm = {
    def mergeCanonicalForms(acc: CanonicalForm, n: CanonicalForm): CanonicalForm = {
      for {
        (p, exp) <- n
        expInAcc: Int = acc.getOrElse(p, 0)
        if expInAcc > 0
      } yield p -> math.min(exp, expInAcc)
    }

    numbers.reduceLeft(mergeCanonicalForms)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val numbers: List[CanonicalForm] = (0 until n).map(_ => readCanonicalForm(reader.next())).toList
    val result: CanonicalForm = calcGCD(numbers)
    println(result.keys.toList.sorted.flatMap(prime => List(prime, result(prime))).mkString(" "))
  }
}
