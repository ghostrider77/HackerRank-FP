package adhoc

object Mangoes {
  import scala.annotation.tailrec
  import scala.collection.mutable.{PriorityQueue => Heap}

  private def readInitialData(line: String): (Int, Long) = {
    val Array(nrFriends, nrMangoes): Array[String] = line.split(" ")
    (nrFriends.toInt, nrMangoes.toLong)
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcMaximalNumberOfGuests(appetite: List[Int], happiness: List[Int], nrFriends: Int, nrMangoes: Long): Int = {
    def mangoesConsumed(k: Int): Long = {
      val heap: Heap[Long] = Heap()(Ordering[Long].reverse)
      for { (a, h) <- appetite.lazyZip(happiness) } heap.enqueue(a + (k - 1) * h.toLong)
      (0 until k).foldLeft(0L) {
        case (acc, _) =>
          val nextElem: Long = heap.dequeue()
          acc + nextElem
      }
    }

    @tailrec
    def binarySearch(start: Int, end: Int): Int = {
      if (start == end) start
      else {
        val mid: Int = if (start + 1 == end) end else (start + end) / 2
        val consumption: Long = mangoesConsumed(mid)
        if (consumption <= nrMangoes) binarySearch(mid, end)
        else binarySearch(start, mid - 1)
      }
    }
    if (appetite.min > nrMangoes) 0 else binarySearch(1, nrFriends)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nrFriends, nrMangoes): (Int, Long) = readInitialData(reader.next())
    val appetite: List[Int] = convertToIntList(reader.next())
    val happiness: List[Int] = convertToIntList(reader.next())
    val result: Int = calcMaximalNumberOfGuests(appetite, happiness, nrFriends, nrMangoes)
    println(result)
  }
}
