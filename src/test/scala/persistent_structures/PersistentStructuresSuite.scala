package persistent_structures

import org.scalatest.{FreeSpec, Matchers}

class PersistentStructuresSuite extends FreeSpec with Matchers {

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
}
