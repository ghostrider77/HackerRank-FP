package recursion

object TreeOfLife {
  import scala.annotation.tailrec

  private val NeighbourhoodSize: Int = 4

  sealed trait CellState
  case object On extends CellState {
    override def toString: String = "X"
  }
  case object Off extends CellState {
    override def toString: String = "."
  }
  case object Unknown extends CellState

  object CellState {
    def apply(c: Char): CellState = c match {
      case '.' => Off
      case 'X' => On
      case _ => Unknown
    }
  }

  sealed trait BinaryTree {
    val state: CellState
  }
  case class Node(state: CellState, parentState: CellState, left: BinaryTree, right: BinaryTree) extends BinaryTree {
    override def toString: String = this match {
      case Node(state, _, left: Node, right: Node) => s"(${left.toString} $state ${right.toString})"
      case _ => state.toString
    }
  }
  case object Leaf extends BinaryTree {
    val state: CellState = Off

    override def toString: String = ""
  }

  def buildTree(encoding: List[Char]): BinaryTree = {
    @tailrec
    def loop(xs: List[Char], stack: List[Node]): BinaryTree = xs match {
      case Nil => stack.head
      case char :: xss => char match {
        case '.' | 'X' => loop(xss, Node(CellState(char), Off, Leaf, Leaf) :: stack)
        case ')' =>
          stack.splitAt(3) match {
            case (List(right, center, left), rest) =>
              val leftChild: Node = left.copy(parentState = center.state)
              val rightChild: Node = right.copy(parentState = center.state)
              val parent: Node = center.copy(left = leftChild, right = rightChild)
              loop(xss, parent :: rest)
            case _ => throw new Exception("Malformed input.")
          }
        case _ => loop(xss, stack)
      }
    }

    loop(encoding, Nil)
  }

  private def parseRule(rule: Int): Map[List[CellState], CellState] = {
    val size: Int = math.pow(2, NeighbourhoodSize).toInt

    def decoding(n: Int, nrBytes: Int): List[CellState] =
      (0 until nrBytes)
        .foldLeft((List[Int](), n)){ case ((acc, k), _) => (k % 2 :: acc, k / 2) }
        ._1
        .map(b => if (b == 1) On else Off)

    val decodedRule: List[CellState] = decoding(rule, size)
    (for {
      (state, n) <- decodedRule.reverse.zipWithIndex
    } yield decoding(n, NeighbourhoodSize) -> state).toMap
  }

  private def transform(tree: BinaryTree, rule: Map[List[CellState], CellState]): BinaryTree = tree match {
    case Leaf => Leaf
    case Node(state, parentState, left, right) =>
      val nextState: CellState = rule(List(parentState, left.state, state, right.state))
      (transform(left, rule), transform(right, rule)) match {
        case (tl: Node, tr: Node) =>
          Node(nextState, parentState, tl.copy(parentState = nextState), tr.copy(parentState = nextState))
        case (tl, tr) => Node(nextState, parentState, tl, tr)
      }
  }

  @tailrec
  private def walkDown(tree: BinaryTree, path: List[Char]): CellState = tree match {
    case Node(state, _, left, right) => path match {
      case Nil => state
      case char :: rest => char match {
        case '>' => walkDown(right, rest)
        case '<' => walkDown(left, rest)
        case _ => throw new Error("Unknown character in path.")
      }
    }
    case _ => throw new Error("Leaf has default state only.")
  }

  private def readQueries(reader: Iterator[String], nrQueries: Int): List[(Int, List[Char])] = {
    val pathBoundaryCharacters: Set[Char] = Set('[', ']')
    reader
      .take(nrQueries)
      .map(_.split(" ").toList)
      .collect{
        case List(relativePosition, path) =>
          (relativePosition.toInt, path.toList.filterNot(pathBoundaryCharacters.contains))
      }
      .toList
  }

  private def reorderQueries(queries: List[(Int, List[Char])]): (List[(Int, List[Char])], List[Int]) = {
    @tailrec
    def loop(acc: List[(Int, List[Char])], qs: List[(Int, List[Char])], pos: Int): List[(Int, List[Char])] = qs match {
      case Nil => acc.reverse
      case (relPos, path) :: qss => loop((pos + relPos, path) :: acc, qss, pos + relPos)
    }

    val absolutePositions: List[(Int, List[Char])] = loop(Nil, queries, 0)
    absolutePositions.zipWithIndex.sortBy{ case ((pos, _), _) => pos }.unzip
  }

  private def moveForward(tree: BinaryTree, rule: Map[List[CellState], CellState], step: Int): BinaryTree =
    (0 until step).foldLeft(tree)((currentTree, _) => transform(currentTree, rule))

  def runSimulation(tree: BinaryTree, encodedRule: Int, queries: List[(Int, List[Char])]): List[CellState] = {
    val rule: Map[List[CellState], CellState] = parseRule(encodedRule)
    val (orderedQueries, originalOrder): (List[(Int, List[Char])], List[Int]) = reorderQueries(queries)

    @tailrec
    def loop(acc: List[CellState],
             qs: List[(Int, List[Char])],
             pos: Int,
             currentTree: BinaryTree): List[CellState] = qs match {
      case Nil => acc.reverse
      case (absPos, path) :: qss =>
        val step: Int = absPos - pos
        val nextTree: BinaryTree = moveForward(currentTree, rule, step)
        val state: CellState = walkDown(nextTree, path)
        loop(state :: acc, qss, absPos, nextTree)
    }

    val reorderedResult: List[CellState] = loop(Nil, orderedQueries, 0, tree)
    reorderedResult.zip(originalOrder).sortBy{ case (_, k) => k }.map{ case (state, _) => state }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val encodedRule: Int = reader.next().toInt
    val tree: BinaryTree = buildTree(reader.next().toList)
    val nrQueries: Int = reader.next().toInt
    val queries: List[(Int, List[Char])] = readQueries(reader, nrQueries)
    val result: List[CellState] = runSimulation(tree, encodedRule, queries)
    result.foreach(println)
  }
}
