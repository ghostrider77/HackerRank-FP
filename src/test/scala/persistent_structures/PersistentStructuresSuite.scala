package persistent_structures

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PersistentStructuresSuite extends AnyFreeSpec with Matchers {

  "MinimumMultiple" - {
    import MinimumMultiple.{Operation, calcMinimumMultiples, readOperation}

    "should calculate the minimum multiple of given ranges of an array with possible updates" - {
      "test case 1" in {
        val n: Int = 5
        val array: Vector[Int] = Vector(2, 5, 6, 1, 9)
        val operations: List[Operation] =
          List("Q 0 4", "U 1 2", "Q 0 2", "Q 3 4", "Q 2 4", "U 3 8", "Q 2 3").map(readOperation)
        calcMinimumMultiples(array, n, operations) shouldEqual List(90, 30, 9, 18, 24)
      }

      "test case 2" in {
        val n: Int = 3
        val array: Vector[Int] = Vector(3, 64, 100)
        val operations: List[Operation] =
          List("Q 1 2", "U 2 100", "U 2 100", "U 2 100", "U 2 100", "Q 1 2").map(readOperation)
        calcMinimumMultiples(array, n, operations) shouldEqual List(1600, 999999937)
      }
    }
  }

  "MessyMedians" - {
    import MessyMedians.calcRunningMedians

    "should calculate running median of a stream of integers with additional rollbacks" - {
      "test case 1" in {
        val n: Int = 10
        val input: List[Int] = List(1, 5, -2, 3, 2, 5, 4, -7, 2, -3)
        calcRunningMedians(input, n) shouldEqual List(1, 1, 1, 1, 2, 2, 3, 1, 1, 3)
      }

      "test case 2" in {
        val n: Int = 10
        val input: List[Int] = List(1000093, 1000080, 1000055, 1000092, 1000039, -1, -3, -5, -7, -9)
        calcRunningMedians(input, n) shouldEqual
          List(1000093, 1000080, 1000080, 1000080, 1000080, 1000080, 1000080, 1000080, 1000080, 1000093)
      }

      "test case 3" in {
        val n: Int = 20
        val input: List[Int] = List(19, 19, -1, 18, 19, 17, -6, 18, -1, -5, -4, 16, 17, 17, 16, 19, 20, 18, 15, -17)
        calcRunningMedians(input, n) shouldEqual
          List(19, 19, 19, 19, 19, 19, 19, 18, 18, 19, 19, 16, 17, 17, 17, 17, 17, 17, 17, 19)
      }
    }
  }
}
