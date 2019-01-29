package functional_structures

object ValidBST {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def isValidBST(preorderTraversal: List[Int]): Boolean = {
    def removeFromStack(stack: List[Int], elem: Int, root: Int): (Int, List[Int]) = {
      @tailrec
      def loop(xs: List[Int], currentRoot: Int): (Int, List[Int]) = xs match {
        case x :: xss if x < elem => loop(xss, x)
        case _ => (currentRoot, xs)
      }
      loop(stack, root)
    }

    @tailrec
    def loop(stack: List[Int], xs: List[Int], root: Int): Boolean = xs match {
      case Nil => true
      case x :: xss =>
        if (x < root) false
        else {
          val (updatedRoot, remainingStack): (Int, List[Int]) = removeFromStack(stack, x, root)
          loop(x :: remainingStack, xss, updatedRoot)
        }
    }

    loop(Nil, preorderTraversal, 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val data: List[List[Int]] = (for {_ <- 0 until numberOfTestCases} yield {
      val _: Int = reader.next().toInt
      val lst: List[Int] = convertToIntList(reader.next())
      lst
    }).toList
    val results: List[Boolean] = data.map(isValidBST)
    results.foreach(verdict => if (verdict) println("YES") else println("NO"))
  }
}
