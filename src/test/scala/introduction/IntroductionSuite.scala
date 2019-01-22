package introduction

import org.scalatest.{FreeSpec, Matchers, Inspectors}

class IntroductionSuite extends FreeSpec with Matchers with Inspectors {

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

  "ArrayOfNElements" - {
    import ArrayOfNElements.createList

    "should create a list of given length" in {
      val n: Int = 10
      createList(10) should have length n
    }
  }

  "ReverseAList" - {
    import ReverseAList.reverse

    "should reverse a list" in {
      val lst: List[Int] = List(19, 22, 3, 28, 26, 17, 18, 4, 28, 0)
      reverse(lst) shouldEqual List(0, 28, 4, 18, 17, 26, 28, 3, 22, 19)
    }
  }

  "SumOfOddElements" - {
    import SumOfOddElements.calcSumOfOddElems

    "should sum the odd elements in a list" in {
      val lst: List[Int] = List(3, 2, 4, 6, 5, 7, 8, 0, 1)
      calcSumOfOddElems(lst) shouldEqual 16
    }
  }

  "ListLength" - {
    "should calculate the length of a list" in {
      val lst: List[Int] = List(2, 5, 1, 4, 3, 7, 8, 6, 0, 9)
      ListLength.length(lst) shouldEqual 10
    }
  }

  "UpdateList" - {
    import UpdateList.replaceWithAbsoluteValue

    "should replace each element with its absolute value" in {
      val lst: List[Int] = List(2, -4, 3, -1, 23, -4, -54)
      replaceWithAbsoluteValue(lst) shouldEqual List(2, 4, 3, 1, 23, 4, 54)
    }
  }

  "EvaluatingExp" - {
    import EvaluatingExp.exponential

    "should calculate e^x based on the first 10 terms of its Taylor-series" in {
      val exponents: List[Double] = List(20.0, 5.0, 0.5, -0.5)
      val result: List[Double] = exponents.map(exponential)
      val expectedResult: List[Double] = List(2423600.1887, 143.6895, 1.6487, 0.6065)
      forAll(result.zip(expectedResult)) {
        case (x, y) => x shouldBe (y +- 0.1)
      }
    }
  }
}
