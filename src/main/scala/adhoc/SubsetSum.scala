package adhoc

object SubsetSum {
  import scala.collection.Searching.{Found, InsertionPoint}

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  def calcMinimumSubsetSizes(vector: Vector[Int], length: Int, queries: List[Long]): List[Int] = {
    val partialSums: Vector[Long] = vector.sorted(Ordering[Int].reverse).scanLeft(0L){_ + _}
    def findSmallestSubsetIndex(query: Long): Int = partialSums.search(query) match {
      case Found(ix) => ix
      case InsertionPoint(ix) if ix <= length => ix
      case _ => -1
    }
    queries.map(findSmallestSubsetIndex)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val vector: Vector[Int] = convertToIntVector(reader.next())
    val nrCases: Int = reader.next().toInt
    val queries: List[Long] = (for { _ <- 0 until nrCases } yield reader.next().toLong).toList
    val results: List[Int] = calcMinimumSubsetSizes(vector, n, queries)
    results.foreach(println)
  }
}
