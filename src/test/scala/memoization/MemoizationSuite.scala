package memoization

import org.scalatest.{FreeSpec, Matchers}

class MemoizationSuite extends FreeSpec with Matchers {

  "NumberOfBinarySearchTrees" - {
    import NumberOfBinarySearchTrees.calcNumberOfBinarySearchTrees

    "should calculate the number of binary search trees modulo a large prime" in {
      val ns: List[Int] = List(1, 2, 3, 4, 100)
      calcNumberOfBinarySearchTrees(ns) shouldEqual List(1, 2, 5, 14, 25666077)
    }
  }

  "PentagonalNumbers" - {
    import PentagonalNumbers.pentagonalNumber

    "should calculate the number of points a pentagon with given size consists of" in {
      val ns: List[Long] = List(1, 2, 3, 4, 5)
      ns.map(pentagonalNumber) shouldEqual List(1, 5, 12, 22, 35)
    }
  }
}
