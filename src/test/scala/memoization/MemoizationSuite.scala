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

  "BitterChocolate" - {
    import BitterChocolate.{Board, doesFirstPlayerWin}

    "should determine if the first player wins the bitter chocolate game" in {
      val boardSizes: List[Board] =
        List(Board(upper = 1, middle = 1, lower = 1), Board(upper = 1, middle = 2, lower = 2))
      boardSizes.map(doesFirstPlayerWin) shouldEqual List(true, false)
    }
  }

  "Fibonacci" - {
    import Fibonacci.calcModuloFibonacci

    "should calculate the Fibonacci number modulo of a large prime" in {
      val ns: List[Int] = List(0, 1, 5, 10, 100)
      calcModuloFibonacci(ns) shouldEqual List(0, 1, 5, 55, 24278230)
    }
  }

  "BangaloreBank" - {
    import BangaloreBank.calcMinimumTypingTime

    "should calculate the minimum time required to rewrite the bank account number" in {
      val inputs: List[List[Int]] =
        List(
          List(1, 2),
          List(1, 10, 3),
          List(2, 5, 1)
        )
      inputs.map(calcMinimumTypingTime) shouldEqual List(2, 5, 4)
    }
  }
}
