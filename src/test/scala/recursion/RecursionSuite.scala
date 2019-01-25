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
}
