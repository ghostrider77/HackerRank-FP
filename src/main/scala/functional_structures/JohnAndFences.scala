package functional_structures

object JohnAndFences {
  import scala.annotation.tailrec

  final case class Fence(index: Int, height: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def calcRectangleArea(fences: List[Fence], height: Int, currentIndex: Int): Int = fences match {
    case Nil => currentIndex * height
    case Fence(ix, _) :: _ => height * (currentIndex - ix - 1)
  }

  def calcLargestRectangle(heights: List[Int], nrFences: Int): Int = {
    @tailrec
    def loop(hs: List[Int], ix: Int, stack: List[Fence], largestArea: Int): (List[Fence], Int) = hs match {
      case Nil => (stack, largestArea)
      case h :: hss => stack match {
        case Nil => loop(hss, ix + 1, Fence(ix, h) :: stack, largestArea)
        case Fence(_, fenceHeight) :: fences =>
          if (fenceHeight <= h) loop(hss, ix + 1, Fence(ix, h) :: stack, largestArea)
          else {
            val area: Int = calcRectangleArea(fences, fenceHeight, ix)
            loop(hs, ix, fences, math.max(area, largestArea))
          }
      }
    }

    @tailrec
    def processRemainingStack(stack: List[Fence], largestArea: Int): Int = stack match {
      case Nil => largestArea
      case Fence(_, h) :: fences =>
        val area: Int = calcRectangleArea(fences, h, nrFences)
        processRemainingStack(fences, math.max(area, largestArea))
      }

    val (stack, largestArea): (List[Fence], Int) = loop(heights, 0, Nil, 0)
    processRemainingStack(stack, largestArea)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrFences: Int = reader.next().toInt
    val heights: List[Int] = convertToIntList(reader.next())
    val result: Int = calcLargestRectangle(heights, nrFences)
    println(result)
  }
}
