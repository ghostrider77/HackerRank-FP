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
      calcPascalsTriangle(n) shouldEqual List(List(1), List(1, 1), List(1, 2, 1), List(1, 3, 3, 1))
    }
  }

  "StringMingling" - {
    import StringMingling.interleave

    "should interleave 2 strings of the same length" in {
      interleave("abcde", "pqrst") shouldEqual "apbqcrdset"
      interleave("hacker", "ranker") shouldEqual "hraacnkkeerr"
    }
  }

  "StringOPermute" - {
    import StringOPermute.permute

    "should swap each consecutive character-pairs" in {
      permute("abcdpqrs") shouldEqual "badcqpsr"
      permute("az") shouldEqual "za"
    }
  }

  "ConvexHull" - {
    import ConvexHull.{Point, calcConvexHullPerimeter}

    "should calculate the perimeter of the convex hull of a polygon" - {
      "test case 1" in {
        val points: List[Point] = List(Point(1, 1), Point(2, 5), Point(3, 3), Point(5, 3), Point(3, 2), Point(2, 2))
        calcConvexHullPerimeter(points) shouldBe (12.200792856 +- 0.2)
      }

      "test case 2" in {
        val points: List[Point] =
          List(
            Point(3, 0),
            Point(2, 0),
            Point(1, 0),
            Point(0, 0),
            Point(1, 1),
            Point(2, 1),
            Point(3, 1),
            Point(3, 2),
            Point(2, 2),
            Point(3, 3)
          )
        val expectedPerimeter: Double = 6 + 3*math.sqrt(2.0)
        calcConvexHullPerimeter(points) shouldBe (expectedPerimeter +- 0.2)
      }
    }
  }
}
