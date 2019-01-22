package introduction

import org.scalatest.{FreeSpec, Matchers}

class IntroductionSuite extends FreeSpec with Matchers {

  "SolveMeFirst" - {
    import SolveMeFirst.sumTwoInts

    "should calculate the sum of two integers" in {
      val a: Int = 2
      val b: Int = 3
      sumTwoInts(a, b) shouldEqual 5
    }
  }
}
