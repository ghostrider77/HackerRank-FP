package persistent_structures

object MessyMedians {
  import scala.annotation.tailrec

  sealed abstract class SkewHeap[+T] {
    val size: Int
  }

  final case object Empty extends SkewHeap[Nothing] {
    val size: Int = 0
  }

  final case class Node[+T](elem: T, size: Int, left: SkewHeap[T], right: SkewHeap[T]) extends SkewHeap[T]

  object SkewHeap {
    private val errorMsg: String = "Invalid heap operation"

    def singleton[T](elem: T): SkewHeap[T] = Node(elem, 1, Empty, Empty)

    def merge[T](heap1: SkewHeap[T], heap2: SkewHeap[T])(implicit ord: Ordering[T]): SkewHeap[T] =
      (heap1, heap2) match {
        case (Empty, _) => heap2
        case (_, Empty) => heap1
        case (h1 @ Node(r1, s1, left1, right1), h2 @ Node(r2, s2, left2, right2)) =>
          val size: Int = s1 + s2
          if (ord.lt(r1, r2)) Node(r1, size, merge(h2, right1), left1)
          else Node(r2, size, merge(h1, right2), left2)
      }

    def insert[T](elem: T, heap: SkewHeap[T])(implicit ord: Ordering[T]): SkewHeap[T] = merge(singleton(elem), heap)

    def top[T](heap: SkewHeap[T]): T = heap match {
      case Empty => throw new Exception(errorMsg)
      case Node(elem, _, _, _) => elem
    }

    def removeTop[T](heap: SkewHeap[T])(implicit ord: Ordering[T]): SkewHeap[T] = heap match {
      case Empty => throw new Exception(errorMsg)
      case Node(_, _, left, right) => merge(left, right)
    }
  }

  class OrderedHeap[T](ord: Ordering[T]) {
    val singleton: T => SkewHeap[T] = SkewHeap.singleton
    val merge: (SkewHeap[T], SkewHeap[T]) => SkewHeap[T] = SkewHeap.merge(_, _)(ord)
    val insert: (T, SkewHeap[T]) => SkewHeap[T] = SkewHeap.insert(_, _)(ord)
    val top: SkewHeap[T] => T = SkewHeap.top
    val removeTop: SkewHeap[T] => SkewHeap[T] = SkewHeap.removeTop(_)(ord)
  }

  final case class State(median: Int, minHeap: SkewHeap[Int], maxHeap: SkewHeap[Int])

  private def calcMedian(minSkewHeap: SkewHeap[Int],
                         maxSkewHeap: SkewHeap[Int],
                         getTop: SkewHeap[Int] => Int): Int = {
    if (minSkewHeap.size > maxSkewHeap.size) getTop(minSkewHeap)
    else if (minSkewHeap.size < maxSkewHeap.size) getTop(maxSkewHeap)
    else getTop(maxSkewHeap)
  }

  private def addElemToHeaps(x: Int, state: State, minHeap: OrderedHeap[Int], maxHeap: OrderedHeap[Int]): State = {
    val State(median, minSkewHeap, maxSkewHeap) = state
    val (updatedMinSkewHeap, updatedMaxSkewHeap): (SkewHeap[Int], SkewHeap[Int]) =
      if (x < median) (minSkewHeap, maxHeap.insert(x, maxSkewHeap))
      else (minHeap.insert(x, minSkewHeap), maxSkewHeap)
    val (balancedMinSkewHeap, balancedMaxSkewHeap): (SkewHeap[Int], SkewHeap[Int]) =
      if (updatedMinSkewHeap.size + 1 < updatedMaxSkewHeap.size) {
        val maxElem: Int = maxHeap.top(updatedMaxSkewHeap)
        val reducedMaxSkewHeap: SkewHeap[Int] = maxHeap.removeTop(updatedMaxSkewHeap)
        (minHeap.insert(maxElem, updatedMinSkewHeap), reducedMaxSkewHeap)
      } else if (updatedMaxSkewHeap.size + 1 < updatedMinSkewHeap.size) {
        val minElem: Int = minHeap.top(updatedMinSkewHeap)
        val reducedMinSkewHeap: SkewHeap[Int] = minHeap.removeTop(updatedMinSkewHeap)
        (reducedMinSkewHeap, maxHeap.insert(minElem, updatedMaxSkewHeap))
      } else (updatedMinSkewHeap, updatedMaxSkewHeap)
    val nextMedian: Int = calcMedian(balancedMinSkewHeap, balancedMaxSkewHeap, minHeap.top)
    State(nextMedian, balancedMinSkewHeap, balancedMaxSkewHeap)
  }

  def calcRunningMedians(operations: List[Int], n: Int): List[Int] = {
    val minHeap = new OrderedHeap(Ordering[Int])
    val maxHeap = new OrderedHeap(Ordering[Int].reverse)

    val states: Array[State] = new Array[State](n)
    val first: Int = operations.head
    val currentState = State(first, minHeap.insert(first, Empty), Empty)
    states(0) = currentState

    @tailrec
    def loop(currentIx: Int, currentState: State, xs: List[Int], runningMedians: List[Int]): List[Int] = xs match {
      case Nil => runningMedians.reverse
      case x :: xss =>
        val nextIx: Int = currentIx + 1
        val nextState: State =
          if (x >= 0) addElemToHeaps(x, currentState, minHeap, maxHeap) else states(currentIx + x + 1)
        states(nextIx) = nextState
        loop(nextIx, nextState, xss, nextState.median :: runningMedians)
    }

    loop(0, currentState, operations.tail, List(first))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrLines: Int = reader.next().toInt
    val operations: List[Int] = reader.take(nrLines).map(_.toInt).toList
    val result: List[Int] = calcRunningMedians(operations, nrLines)
    result.foreach(println)
  }
}
