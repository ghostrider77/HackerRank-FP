package adhoc

object MissingNumbers {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readList(reader: Iterator[String]): List[Int] = {
    val _: Int = reader.next().toInt
    convertToIntList(reader.next())
  }

  private def countElements(lst: List[Int]): Map[Int, Int] = lst.groupBy(identity).mapValues(_.length)

  def getMissingElements(lst1: List[Int], lst2: List[Int]): List[Int] = {
    val valuesAndCounts1: Map[Int, Int] = countElements(lst1)
    val valuesAndCounts2: Map[Int, Int] = countElements(lst2)

    (for {
      (number, count) <- valuesAndCounts2
      if valuesAndCounts1.getOrElse(number, 0) < count
    } yield number).toList.sorted
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val lst1: List[Int] = readList(reader)
    val lst2: List[Int] = readList(reader)
    val result: List[Int] = getMissingElements(lst1, lst2)
    println(result.mkString(" "))
  }
}
