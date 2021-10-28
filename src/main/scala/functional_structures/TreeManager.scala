package functional_structures

//object TreeManager {
//  import scala.annotation.tailrec
//  private val errorMsg: String = "Invalid tree operation"
//
//  sealed trait Node
//  final case class InnerNode(value: Int,
//                             parent: Node,
//                             children: List[Node],
//                             leftSiblings: List[Node],
//                             rightSiblings: List[Node]) extends Node
//  final case object Root extends Node
//
//  private def changeValue(currentNode: Node, x: Int): Node = currentNode match {
//    case n: InnerNode => n.copy(value = x)
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def print(currentNode: Node): Int = currentNode match {
//    case n: InnerNode => n.value
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def visitLeft(currentNode: Node): Node = currentNode match {
//    case InnerNode(_, p, _, InnerNode(x, _, children, _, _) :: ls, right) =>
//      InnerNode(x, p, children, ls, currentNode :: right)
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def visitRight(currentNode: Node): Node = currentNode match {
//    case InnerNode(_, p, _, left, InnerNode(x, _, children, _, _) :: rs) =>
//      InnerNode(x, p, children, currentNode :: left, rs)
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def visitParent(currentNode: Node): Node = currentNode match {
//    case InnerNode(_, parent, _, left, right) => parent match {
//      case InnerNode(x, p, _, l, r) => InnerNode(x, p, left.reverse ::: currentNode :: right, l, r)
//      case _ => throw new Exception(errorMsg)
//    }
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def visitChild(currentNode: Node, n: Int): Node = {
//    if (n == 1) {
//      currentNode match {
//        case InnerNode(_, _, InnerNode(x, _, children, _, _) :: chs, _, _) =>
//          InnerNode(x, currentNode, children, Nil, chs)
//        case _ => throw new Exception(errorMsg)
//      }
//    } else visitRight(visitChild(currentNode, n - 1))
//  }
//
//  private def insertLeft(currentNode: Node, x: Int): Node = currentNode match {
//    case n @ InnerNode(_, _, _, left, _) => n.copy(leftSiblings = InnerNode(x, currentNode, Nil, Nil, Nil) :: left)
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def insertRight(currentNode: Node, x: Int): Node = currentNode match {
//    case n @ InnerNode(_, _, _, _, right) => n.copy(rightSiblings = InnerNode(x, currentNode, Nil, Nil, Nil) :: right)
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def insertChild(currentNode: Node, x: Int): Node = currentNode match {
//    case n @ InnerNode(_, _, children, _, _) => n.copy(children = InnerNode(x, currentNode, Nil, Nil, Nil) :: children)
//    case _ => throw new Exception(errorMsg)
//  }
//
//  private def delete(currentNode: Node): Node = currentNode match {
//    case InnerNode(_, parent, _, left, right) => parent match {
//      case InnerNode(x, p, _, l, r) => InnerNode(x, p, left.reverse ::: right, l, r)
//      case _ => throw new Exception(errorMsg)
//    }
//    case _ => throw new Exception(errorMsg)
//  }
//
//  sealed trait Operation
//  final case class Change(x: Int) extends Operation
//  final case object Print extends Operation
//  final case object VisitLeft extends Operation
//  final case object VisitRight extends Operation
//  final case object VisitParent extends Operation
//  final case class VisitChild(n: Int) extends Operation
//  final case class InsertLeft(x: Int) extends Operation
//  final case class InsertRight(x: Int) extends Operation
//  final case class InsertChild(x: Int) extends Operation
//  final case object Delete extends Operation
//
//  private[functional_structures] def readOperations(reader: Iterator[String], nrOperations: Int): List[Operation] = {
//    def parseLine(line: String): Operation = line.split(" ").toList match {
//      case List("change", x) => Change(x.toInt)
//      case List("print") => Print
//      case List("visit", "left") => VisitLeft
//      case List("visit", "right") => VisitRight
//      case List("visit", "parent") => VisitParent
//      case List("visit", "child", n) => VisitChild(n.toInt)
//      case List("insert", "left", x) => InsertLeft(x.toInt)
//      case List("insert", "right", x) => InsertRight(x.toInt)
//      case List("insert", "child", x) => InsertChild(x.toInt)
//      case List("delete") => Delete
//    }
//
//    reader.map(parseLine).take(nrOperations).toList
//  }
//
//  def performTreeOperations(operations: List[Operation]): List[Int] = {
//    val topNode: Node = InnerNode(0, Root, Nil, Nil, Nil)
//    @tailrec
//    def loop(ops: List[Operation], currentNode: Node, output: List[Int]): List[Int] = ops match {
//      case Nil => output.reverse
//      case op :: rest =>
//        val (nextNode, message): (Node, Option[Int]) = op match {
//          case Change(x) => (changeValue(currentNode, x), None)
//          case Print => (currentNode, Some(print(currentNode)))
//          case VisitLeft => (visitLeft(currentNode), None)
//          case VisitRight => (visitRight(currentNode), None)
//          case VisitParent => (visitParent(currentNode), None)
//          case VisitChild(n) => (visitChild(currentNode, n), None)
//          case InsertLeft(x) => (insertLeft(currentNode, x), None)
//          case InsertRight(x) => (insertRight(currentNode, x), None)
//          case InsertChild(x) => (insertChild(currentNode, x), None)
//          case Delete => (delete(currentNode), None)
//        }
//        message match {
//          case None => loop(rest, nextNode, output)
//          case Some(m) => loop(rest, nextNode, m :: output)
//        }
//    }
//    loop(operations, topNode, Nil)
//  }
//
//  def main(args: Array[String]): Unit = {
//    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
//    val nrOperations: Int = reader.next().toInt
//    val operations: List[Operation] = readOperations(reader, nrOperations)
//    val results: List[Int] = performTreeOperations(operations)
//    results.foreach(println)
//  }
//}
