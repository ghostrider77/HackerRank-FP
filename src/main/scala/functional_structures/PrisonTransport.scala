package functional_structures

object PrisonTransport {
  import scala.annotation.tailrec

  private class UnionFind(nrPoints: Int) {
    private val parentIndices: Array[Int] = (0 until nrPoints).toArray
    private val ranks: Array[Int] = Array.fill(nrPoints)(0)

    private def changeParentsToRoot(indicesOnPath: List[Int], root: Int): Unit =
      indicesOnPath.foreach(ix => parentIndices(ix) = root)

    def find(childIndex: Int): Int = {
      @tailrec
      def loop(id: Int, parentId: Int, indicesTowardsRoot: List[Int]): (Int, List[Int]) = {
        if (id == parentId) (id, indicesTowardsRoot)
        else loop(parentId, parentIndices(parentId), id :: indicesTowardsRoot)
      }
      val (root, indicesOnPath): (Int, List[Int]) = loop(childIndex, parentIndices(childIndex), Nil)
      changeParentsToRoot(indicesOnPath, root)
      root
    }

    def union(parentIndexP: Int, parentIndexQ: Int): Unit = {
      if (parentIndexP != parentIndexQ) {
        if (ranks(parentIndexP) > ranks(parentIndexQ)) parentIndices(parentIndexQ) = parentIndexP
        else {
          parentIndices(parentIndexP) = parentIndexQ
          if (ranks(parentIndexP) == ranks(parentIndexQ)) ranks(parentIndexQ) += 1
        }
      }
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def calcGroupSizes(nrPrisoners: Int, prisonerPairs: List[(Int, Int)]): List[Int] = {
    val prisoners: UnionFind = new UnionFind(nrPrisoners)
    for { (p, q) <- prisonerPairs } {
      val groupOfP: Int = prisoners.find(p)
      val groupOfQ: Int = prisoners.find(q)
      prisoners.union(groupOfP, groupOfQ)
    }

    val groupSizes: Map[Int, Int] = (0 until nrPrisoners).foldLeft(Map.empty[Int, Int]){
      case (acc, ix) =>
        val root: Int = prisoners.find(ix)
        acc.updated(root, acc.getOrElse(root, 0) + 1)
    }
    groupSizes.values.toList
  }

  def calcMinimalTransportationCost(nrPrisoners: Int, prisonerPairs: List[(Int, Int)]): Int = {
    val componentSizes: List[Int] = calcGroupSizes(nrPrisoners, prisonerPairs)
    componentSizes.foldLeft(0)((acc, size) => acc + math.ceil(math.sqrt(size)).toInt)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPrisoners: Int = reader.next().toInt
    val nrPairs: Int = reader.next().toInt
    val prisonerPairs: List[(Int, Int)] =
      (for { _ <- 0 until nrPairs } yield {
        val List(first, second): List[Int] = convertToIntList(reader.next())
        (first - 1, second - 1)
      }).toList
    val result: Int = calcMinimalTransportationCost(nrPrisoners, prisonerPairs)
    println(result)
  }
}
