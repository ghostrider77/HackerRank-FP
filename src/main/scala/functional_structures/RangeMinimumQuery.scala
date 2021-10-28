package functional_structures

object RangeMinimumQuery {
  private def log2(x: Double): Double = math.log(x) / math.log(2)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(n, m) => (n, m)
    case _ => throw new Exception("Unexpected input data format.")
  }

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
    val (length, nrQueries): (Int, Int) = readParameters(reader.next())
    val array: Vector[Int] = convertToIntList(reader.next()).toVector
    val queries: List[(Int, Int)] =
      reader.take(nrQueries).map(convertToIntList).collect{ case List(i, j) => (i, j)}.toList
    val results: List[Int] = calcSegmentMinimum(array, length, queries)
    results.foreach(println)
  }
}
