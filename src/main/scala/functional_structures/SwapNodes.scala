package functional_structures

object SwapNodes {
  import scala.annotation.tailrec

  sealed trait BinaryTree
  case class Node(key: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree
  case object Leaf extends BinaryTree

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private[functional_structures] def buildBinaryTree(reader: Iterator[String], nrNodes: Int): BinaryTree = {
    val nodes: Vector[(Int, Int)] = (for { _ <- 1 to nrNodes } yield {
      val List(left, right): List[Int] = convertToIntList(reader.next())
      (left, right)
    }).toVector

    def buildTree(k: Int): BinaryTree = {
      if (k == -1) Leaf
      else {
        val (left, right): (Int, Int) = nodes(k - 1)
        Node(key = k, left = buildTree(left), right = buildTree(right))
      }
    }

    buildTree(1)
  }

  private def inorderTraversal(tree: BinaryTree): List[Int] = tree match {
    case Leaf => Nil
    case Node(k, left, right) => inorderTraversal(left) ::: k :: inorderTraversal(right)
  }

  private def swapNodes(tree: BinaryTree, k: Int): BinaryTree = {
    def swap(t: BinaryTree, n: Int): BinaryTree = t match {
      case Leaf => Leaf
      case Node(key, left, right) =>
        if (n == k) Node(key, swap(right, 1), swap(left, 1))
        else Node(key, swap(left, n + 1), swap(right, n + 1))
    }
    swap(tree, 1)
  }

  def performTraversals(tree: BinaryTree, swaps: List[Int]): List[List[Int]] = {
    @tailrec
    def loop(currentTree: BinaryTree, xs: List[Int], acc: List[List[Int]]): List[List[Int]] = xs match {
      case Nil => acc.reverse
      case x :: xss =>
        val swappedTree: BinaryTree = swapNodes(currentTree, x)
        loop(swappedTree, xss, inorderTraversal(swappedTree) :: acc)
    }
    loop(tree, swaps, Nil)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrNodes: Int = reader.next().toInt
    val tree: BinaryTree = buildBinaryTree(reader, nrNodes)
    val nrSwapOperations: Int = reader.next().toInt
    val swaps: List[Int] = (0 until nrSwapOperations).map(_ => reader.next().toInt).toList
    val traversals: List[List[Int]] = performTraversals(tree, swaps)
    traversals.foreach(lst => println(lst.mkString(" ")))
  }
}
