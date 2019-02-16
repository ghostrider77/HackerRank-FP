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

  "Expressions" - {
    import Expressions.findExpression

    "should create an expressions from a given list of integers that divisble by 101" - {
      "test case 1" in {
        val numbers: List[Int] = List(22, 79, 21)
        val n: Int = numbers.length
        findExpression(numbers, n) shouldEqual "22+79*21"
      }

      "test case 2" in {
        val numbers: List[Int] = List(55, 3, 45, 33, 25)
        val n: Int = numbers.length
        findExpression(numbers, n) shouldEqual "55*3+45-33+25"
      }
    }
  }

  "SherlockAndTheMaze" - {
    import SherlockAndTheMaze.calcNumberOfPathsWithAtMostKTurns

    "should calculate the number of roads from top left to bottom right corner containing at most k turns" in {
      calcNumberOfPathsWithAtMostKTurns(1, 1, 0) shouldEqual 1
      calcNumberOfPathsWithAtMostKTurns(2, 2, 0) shouldEqual 0
      calcNumberOfPathsWithAtMostKTurns(2, 2, 3) shouldEqual 2
      calcNumberOfPathsWithAtMostKTurns(2, 3, 1) shouldEqual 2
      calcNumberOfPathsWithAtMostKTurns(4, 4, 4) shouldEqual 18
    }
  }

  "DicePath" - {
    import DicePath.calcSumOfMaximalPaths

    "should calculate the sum of maximal path to the bottom right corner of a grid" in {
      val gridsizes: List[(Int, Int)] = List((2, 2), (1, 2), (2, 1), (3, 3))
      calcSumOfMaximalPaths(gridsizes) shouldEqual List(9, 4, 6, 19)
    }
  }

  "DifferentWays" - {
    import DifferentWays.{CombinationParameters, differentWays}

    "should calculate n choose k modulo a large integer" in {
      val input: List[CombinationParameters] =
        List(
          CombinationParameters(2, 1),
          CombinationParameters(5, 1),
          CombinationParameters(5, 2),
          CombinationParameters(5, 3),
          CombinationParameters(10, 5)
        )
      differentWays(input) shouldEqual List(2, 5, 10, 10, 252)
    }
  }

  "ReverseFactorization" - {
    import ReverseFactorization.calcShortestFactorization

    "should calculate the shortest and lexicographically smallest path to a given integer" - {
      "test case 1" in {
        val target: Int = 12
        val factors: List[Int] = List(2, 3, 4)
        calcShortestFactorization(factors, target) shouldEqual List(1, 3, 12)
      }

      "test case 2" in {
        val target: Int = 15
        val factors: List[Int] = List(2, 10, 6, 9, 11)
        calcShortestFactorization(factors, target) shouldEqual List(-1)
      }

      "test case 3" in {
        val target: Int = 72
        val factors: List[Int] = List(2, 4, 6, 9, 3, 7, 16, 10, 5)
        calcShortestFactorization(factors, target) shouldEqual List(1, 2, 8, 72)
      }

      "test case 4" in {
        val target: Int = 24
        val factors: List[Int] = List(4, 6, 8)
        calcShortestFactorization(factors, target) shouldEqual List(1, 4, 24)
      }

      "test case 5" in {
        val target: Int = 16
        val factors: List[Int] = List(4, 8)
        calcShortestFactorization(factors, target) shouldEqual List(1, 4, 16)
      }

      "test case 6" in {
        val target: Int = 36
        val factors: List[Int] = List(18, 9, 4)
        calcShortestFactorization(factors, target) shouldEqual List(1, 4, 36)
      }
    }
  }
}
