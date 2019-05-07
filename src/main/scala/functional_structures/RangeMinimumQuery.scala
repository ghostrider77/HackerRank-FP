package functional_structures

object RangeMinimumQuery {
  private def log2(x: Double): Double = math.log(x) / math.log(2)

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  def calcSegmentMinimum(array: Vector[Int], n: Int, queries: List[(Int, Int)]): List[Int] = {
    val maxNrNodes: Int = 2 * math.pow(2, math.ceil(log2(n)).toInt).toInt - 1
    val segmentTree: Array[Int] = Array.fill(maxNrNodes)(0)

    def buildTree(nodeIndex: Int, leftEnd: Int, rightEnd: Int): Unit = {
      if (leftEnd == rightEnd) segmentTree(nodeIndex) = array(leftEnd)
      else {
        val leftChildIx: Int = 2 * nodeIndex + 1
        val rightChildIx: Int = leftChildIx + 1
        buildTree(leftChildIx, leftEnd, (leftEnd + rightEnd) / 2)
        buildTree(rightChildIx, (leftEnd + rightEnd) / 2 + 1, rightEnd)
        segmentTree(nodeIndex) = math.min(segmentTree(leftChildIx), segmentTree(rightChildIx))
      }
    }

    def getSegmentMinimum(nodeIndex: Int, leftEnd: Int, rightEnd: Int, i: Int, j: Int): Int = {
      if (i <= leftEnd && j >= rightEnd) segmentTree(nodeIndex)
      else if (i > rightEnd || j < leftEnd) Int.MaxValue
      else {
        math.min(
          getSegmentMinimum(2 * nodeIndex + 1, leftEnd, (leftEnd + rightEnd) / 2, i, j),
          getSegmentMinimum(2 * nodeIndex + 2, (leftEnd + rightEnd) / 2 + 1, rightEnd, i, j)
        )
      }
    }

    buildTree(0, 0, n - 1)
    queries.map{ case (i, j) => getSegmentMinimum(0, 0, n - 1, i, j) }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(length, nrQueries): List[Int] = convertToIntArray(reader.next()).toList
    val array: Vector[Int] = convertToIntArray(reader.next()).toVector
    val queries: List[(Int, Int)] = (for { _ <- 0 until nrQueries }
      yield {
        val List(i, j): List[Int] = convertToIntArray(reader.next()).toList
        (i, j)
      }).toList
    val results: List[Int] = calcSegmentMinimum(array, length, queries)
    results.foreach(println)
  }
}
