package functional_structures

object FightingArmies {
  import scala.annotation.tailrec

  sealed trait PairingHeap
  final case object Empty extends PairingHeap
  final case class PairingTree(root: Int, subheaps: List[PairingHeap]) extends PairingHeap

  private val errorMsg: String = "Invalid heap operation"

  private def findMax(heap: PairingHeap): Int = heap match {
    case Empty => throw new Exception(errorMsg)
    case PairingTree(root, _) => root
  }

  private def merge(heap1: PairingHeap, heap2: PairingHeap): PairingHeap = (heap1, heap2) match {
    case (Empty, _) => heap2
    case (_, Empty) => heap1
    case (h1 @ PairingTree(root1, subheaps1), h2 @ PairingTree(root2, subheaps2)) =>
      if (root1 > root2) PairingTree(root1, h2 :: subheaps1)
      else PairingTree(root2, h1 :: subheaps2)
  }

  private def insert(elem: Int, heap: PairingHeap): PairingHeap = merge(PairingTree(elem, Nil), heap)

  private def removeMax(heap: PairingHeap): PairingHeap = {
    def mergePairs(heaps: List[PairingHeap]): PairingHeap = heaps match {
      case Nil => Empty
      case List(h) => h
      case h1 :: h2 :: hs =>  merge(merge(h1, h2), mergePairs(hs))
    }

    heap match {
      case Empty => throw new Exception(errorMsg)
      case PairingTree(_, subheaps) => mergePairs(subheaps)
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def manageArmies(reader: Iterator[String], nrArmies: Int): List[Int] = {
    val armies: Array[PairingHeap] = Array.fill(nrArmies)(Empty)

    @tailrec
    def loop(acc: List[Int]): List[Int] = {
      if (reader.isEmpty) acc.reverse
      else {
        convertToIntList(reader.next()) match {
          case List(1, armyIx) =>
            loop(findMax(armies(armyIx - 1)) :: acc)
          case List(2, armyIx) =>
            armies(armyIx - 1) = removeMax(armies(armyIx - 1))
            loop(acc)
          case List(3, armyIx, combatAbility) =>
            armies(armyIx - 1) = insert(combatAbility, armies(armyIx - 1))
            loop(acc)
          case List(4, armyIx, armyJy) =>
            armies(armyIx - 1) = merge(armies(armyIx - 1), armies(armyJy - 1))
            armies(armyJy - 1) = Empty
            loop(acc)
          case _ => throw new Exception("impossible army operation")
        }
      }
    }

    loop(Nil)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrArmies, nrEvents): List[Int] = convertToIntList(reader.next())
    val result: List[Int ] = manageArmies(reader.take(nrEvents), nrArmies)
    result.foreach(println)
  }
}
