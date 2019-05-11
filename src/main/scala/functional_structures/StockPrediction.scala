package functional_structures

object StockPrediction {
  import scala.annotation.tailrec

  final case class Query(day: Int, margin: Int)
  final case class MinMax(min: Int, max: Int)

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  private def combine(minmax1: MinMax, minmax2: MinMax): MinMax =
    MinMax(math.min(minmax1.min, minmax2.min), math.max(minmax1.max, minmax2.max))

  private def calcSegmentTree(array: Vector[Int], n: Int): Vector[MinMax] = {
    def log2(x: Double): Double = math.log(x) / math.log(2)
    val maxNrNodes: Int = 2 * math.pow(2, math.ceil(log2(n)).toInt).toInt - 1
    val segmentTree: Array[MinMax] = Array.fill(maxNrNodes)(MinMax(0, Int.MaxValue))

    def buildTree(nodeIndex: Int, leftEnd: Int, rightEnd: Int): Unit = {
      if (leftEnd == rightEnd) {
        val value: Int = array(leftEnd)
        segmentTree(nodeIndex) = MinMax(value, value)
      } else {
        val leftChildIx: Int = 2 * nodeIndex + 1
        val rightChildIx: Int = leftChildIx + 1
        buildTree(leftChildIx, leftEnd, (leftEnd + rightEnd) / 2)
        buildTree(rightChildIx, (leftEnd + rightEnd) / 2 + 1, rightEnd)
        segmentTree(nodeIndex) = combine(segmentTree(leftChildIx), segmentTree(rightChildIx))
      }
    }

    buildTree(nodeIndex = 0, leftEnd = 0, rightEnd = n - 1)
    segmentTree.toVector
  }

  private def getSegmentMinMax(segmentTree: Vector[MinMax], n: Int, i: Int, j: Int): MinMax = {
    def loop(nodeIndex: Int, leftEnd: Int, rightEnd: Int): MinMax = {
      if (i <= leftEnd && j >= rightEnd) segmentTree(nodeIndex)
      else if (i > rightEnd || j < leftEnd) MinMax(Int.MaxValue, Int.MinValue)
      else combine(
        loop(2 * nodeIndex + 1, leftEnd, (leftEnd + rightEnd) / 2),
        loop(2 * nodeIndex + 2, (leftEnd + rightEnd) / 2 + 1, rightEnd)
      )
    }

    loop(0, 0, n - 1)
  }

  private def calcSubarrayLength(prices: Vector[Int], n: Int, segmentTree: Vector[MinMax], query: Query): Int = {
    val Query(day, margin) = query
    val price: Int = prices(day)
    val upperLimit: Int = price + margin

    @tailrec
    def binarySearchRight(left: Int, right: Int): Int = {
      if (left == right) left
      else {
        val mid: Int = if (left + 1 == right) right else (left + right) / 2
        val MinMax(minimum, maximum) = getSegmentMinMax(segmentTree, n, day, mid)
        if (minimum >= price && maximum <= upperLimit) binarySearchRight(mid, right)
        else binarySearchRight(left, mid - 1)
      }
    }

    @tailrec
    def binarySearchLeft(left: Int, right: Int): Int = {
      if (left == right) left
      else {
        val mid: Int = (left + right) / 2
        val MinMax(minimum, maximum) = getSegmentMinMax(segmentTree, n, mid, day)
        if (minimum >= price && maximum <= upperLimit) binarySearchLeft(left, mid)
        else binarySearchLeft(mid + 1, right)
      }
    }

    val right: Int = binarySearchRight(day, n - 1)
    val left: Int = binarySearchLeft(0, day)
    right - left + 1
  }

  def calcLengthOfLongestSubarrays(prices: Vector[Int], n: Int, queries: List[Query]): List[Int] = {
    val segmentTree: Vector[MinMax] = calcSegmentTree(prices, n)
    queries.map(calcSubarrayLength(prices, n, segmentTree, _))
  }

  def main(args: Array[String]) {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val prices: Vector[Int] = convertToIntArray(reader.next()).toVector
    val nrQueries: Int = reader.next().toInt
    val queries: List[Query] = (for { _ <- 0 until nrQueries } yield {
      val List(d, margin): List[Int] = convertToIntArray(reader.next()).toList
      Query(d, margin)
    }).toList
    val result: List[Int] = calcLengthOfLongestSubarrays(prices, n, queries)
    result.foreach(println)
  }
}
