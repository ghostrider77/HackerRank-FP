package memoization

object ReverseFactorization {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue}

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def breadthFirstSearch(factors: List[Long], target: Int): Map[Long, Long] = {
    val queue: MutableQueue[Long] = MutableQueue(1L)

    def addProduct(elem: Long)(acc: Map[Long, Long], p: Long): Map[Long, Long] = {
      val product: Long = elem * p
      if (product > target || acc.contains(product)) acc
      else {
        queue.enqueue(product)
        acc + (product -> elem)
      }
    }

    @tailrec
    def loop(arrivedFrom: Map[Long, Long]): Map[Long, Long] = {
      if (queue.isEmpty || arrivedFrom.contains(target)) arrivedFrom
      else {
        val current: Long = queue.dequeue()
        val updated: Map[Long, Long] = factors.foldLeft(arrivedFrom)(addProduct(current))
        loop(updated)
      }
    }

    loop(Map(1L -> 0L))
  }

  private def findFactorizationSteps(backTrack: Map[Long, Long], n: Int): List[Long] = {
    @tailrec
    def loop(path: List[Long], currentElem: Long): List[Long] = {
      if (currentElem == 0) path
      else {
        val previousElem: Long = backTrack(currentElem)
        loop(currentElem :: path, previousElem)
      }
    }

    if (!backTrack.contains(n)) List(-1)
    else loop(Nil, n)
  }

  def calcShortestFactorization(factors: List[Int], n: Int): List[Long] = {
    val sortedFactors: List[Long] = factors.sorted.map(_.toLong)
    val backTrack: Map[Long, Long] = breadthFirstSearch(sortedFactors, n)
    findFactorizationSteps(backTrack, n)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(n, _): List[Int] = convertToIntList(reader.next())
    val factors: List[Int] = convertToIntList(reader.next())
    val result: List[Long] = calcShortestFactorization(factors, n)
    println(result.mkString(" "))
  }
}
