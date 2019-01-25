package recursion

import org.scalatest.{FreeSpec, Matchers, Inspectors}

class RecursionSuite extends FreeSpec with Matchers with Inspectors {
  "ComputingGCD" - {
    import ComputingGCD.calcGCD

    "should calculate the greatest common divisor of two integers" in {
      calcGCD(1, 5) shouldEqual 1
      calcGCD(100, 10) shouldEqual 10
      calcGCD(22, 131) shouldEqual 1
    }
  }

  "Fibonacci" - {
    import Fibonacci.fibonacci

    "should calculate the Fibonacci numbers" in {
      fibonacci(1) shouldEqual 0
      fibonacci(2) shouldEqual 1
      fibonacci(3) shouldEqual 1
      fibonacci(7) shouldEqual 8
    }
  }

  "PascalsTriangle" - {
    import PascalsTriangle.calcPascalsTriangle

    "should calculate the first few rows of the Pascal's triangle" in {
      val n: Int = 4
      calcPascalsTriangle(4) shouldEqual List(List(1), List(1, 1), List(1, 2, 1), List(1, 3, 3, 1))
    }
  }
}
