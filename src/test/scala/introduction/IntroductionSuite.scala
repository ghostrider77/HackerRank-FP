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

  "RevolvingCurve" - {
    import RevolvingCurve.{calcIntegral, calcVolume}

    "should calculate the integral and the volume of revolution of a polynomial" in {
      val coefficients: List[Int] = List(1)
      val exponents: List[Int] = List(2)
      val leftEnd: Int = 0
      val rightEnd: Int = 1
      calcIntegral(coefficients, exponents, leftEnd, rightEnd) shouldEqual (0.33333 +- 0.01)
      calcVolume(coefficients, exponents, leftEnd, rightEnd) shouldEqual (0.62831 +- 0.01)
    }
  }

  "FunctionOrNot" - {
    import FunctionOrNot.{Pair, isFunction}

    "should determine if a given list of (x, y) pairs determine a function or not" in {
      val lst1: List[Pair] = List(Pair(1, 1), Pair(2, 2), Pair(3, 3))
      val lst2: List[Pair] = List(Pair(1, 2), Pair(2, 4), Pair(3, 6), Pair(4, 8))
      val lst3: List[Pair] = List(Pair(1, 10), Pair(2, 20), Pair(1, 10))
      val lst4: List[Pair] = List(Pair(1, 10), Pair(2, 20), Pair(1, 30))

      isFunction(lst1) shouldBe true
      isFunction(lst2) shouldBe true
      isFunction(lst3) shouldBe true
      isFunction(lst4) shouldBe false
    }
  }

  "PolygonPerimeter" - {
    import PolygonPerimeter.{Point, calcPolygonPerimeter}

    "should calculate the perimeter of a polygon given by its vertices counter-clockwise" in {
      val points: List[Point] = List(Point(0, 0), Point(0, 1), Point(1, 1), Point(1, 0))
      calcPolygonPerimeter(points) shouldBe (4.0 +- 0.01)
    }
  }

  "PolygonArea" - {
    import PolygonArea.{Point, calcPolygonArea}

    "should calculate the area of a polygon given by its vertices counter-clockwise" in {
      val points: List[Point] = List(Point(0, 0), Point(0, 1), Point(1, 1), Point(1, 0))
      calcPolygonArea(points) shouldBe (1.0 +- 0.01)
    }
  }
}
