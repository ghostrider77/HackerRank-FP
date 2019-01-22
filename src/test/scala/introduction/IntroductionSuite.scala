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

  "ListReplication" - {
    import ListReplication.replicate

    "should replicate each element of a list n times" in {
      val n: Int = 3
      val lst: List[Int] = List(1, 2, 3, 4)
      replicate(n, lst) shouldEqual List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
    }
  }

  "FilterArray" - {
    import FilterArray.filter

    "should keep those elements of a list that are smaller than a given value" in {
      val n: Int = 3
      val lst: List[Int] = List(10, 9, 8, 2, 7, 5, 1, 3, 0)
      filter(n, lst) shouldEqual List(2, 1, 0)
    }
  }

  "FilterPositionsInList" - {
    import FilterPositionsInList.filterOddPositions

    "should remove integers at odd positions" in {
      val lst: List[Int] = List(2, 5, 3, 4, 6, 7, 9, 8)
      filterOddPositions(lst) shouldEqual List(5, 4, 7, 8)
    }
  }
}
