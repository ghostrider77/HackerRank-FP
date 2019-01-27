package recursion

object FilterElements {
  final case class TestCase(sequence: List[Int], k: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def filterElements(testCase: TestCase): List[Int] = {
    val TestCase(lst, k) = testCase
    val uniqueElems: List[Int] = lst.distinct
    val counts: Map[Int, Int] = lst.groupBy(identity).mapValues(_.length)
    val filteredElems: List[Int] = for {
      elem <- uniqueElems
      count = counts.getOrElse(elem, 0)
      if count >= k
    } yield elem
    if (filteredElems.isEmpty) List(-1)
    else filteredElems
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val testCases: List[TestCase] = (for { _ <- 0 until n } yield {
      val List(_, k): List[Int] = convertToIntList(reader.next())
      val seq: List[Int] = convertToIntList(reader.next())
      TestCase(seq, k)
    }).toList
    val result: List[List[Int]] = testCases.map(filterElements)
    result.foreach(lst => println(lst.mkString))
  }
}
