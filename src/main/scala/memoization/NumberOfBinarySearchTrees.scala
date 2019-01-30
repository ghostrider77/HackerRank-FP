package memoization

object NumberOfBinarySearchTrees {
  import collection.mutable.{Map => MutableMap}

  private val Modulus: Int = 1e8.toInt + 7

  def calcNumberOfBinarySearchTrees(nValues: List[Int]): List[Long] = {
    val dict: MutableMap[Int, Long] = MutableMap(0 -> 1L, 1 -> 1L)

    def calcNrBST(k: Int): Long = {
      def Fk: Long =
        (0 until k)
          .map(i => (calcNrBST(i) * calcNrBST(k - 1 - i)) % Modulus)
          .foldLeft(0L)((s, elem) => (s + elem) % Modulus)
      dict.getOrElseUpdate(k, Fk)
    }

    nValues.map(calcNrBST)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val numberOfNodes: List[Int] = (for { _ <- 0 until numberOfTestCases } yield reader.next().toInt).toList
    val result: List[Long] = calcNumberOfBinarySearchTrees(numberOfNodes)
    result.foreach(println)
  }
}
