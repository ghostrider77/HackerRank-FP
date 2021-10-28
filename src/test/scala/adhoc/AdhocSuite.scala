package adhoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inspectors

class AdhocSuite extends AnyFreeSpec with Matchers with Inspectors {

  "JumpingBunnies" - {
    import JumpingBunnies.calcLeastCommonMultiple

    "should calculate the least common multiple of jumping distances" in {
      calcLeastCommonMultiple(List(2, 3, 4)) shouldEqual 12
      calcLeastCommonMultiple(List(1, 3)) shouldEqual 3
      calcLeastCommonMultiple(List(1, 2, 3, 16, 8, 4)) shouldEqual 48
    }
  }

  "RotateString" - {
    import RotateString.getRotations

    "should retrieve all rotations of each string in a list" in {
      val strings = List("abc", "abcde", "abab", "aaa", "z")
      strings.map(getRotations(_).toList) shouldEqual
        List(
          List("bca", "cab", "abc"),
          List("bcdea", "cdeab", "deabc", "eabcd", "abcde"),
          List("baba", "abab", "baba", "abab"),
          List("aaa", "aaa", "aaa"),
          List("z")
        )
    }
  }

  "KunduAndBubbleWrap" - {
    import KunduAndBubbleWrap.calculateExpectedNumberOfTrials

    "should calculate the expected number of steps for popping out all bubbles" in {
      val inputs: List[(Int, Int)] = List((1, 1), (1, 2), (2, 2))
      val results: List[Double] = inputs.map{ case (n, m) => calculateExpectedNumberOfTrials(n * m) }
      val expectedResults: List[Double] = List(1.0, 3.0, 8.333333)
      forAll (results.zip(expectedResults)) {
        case (result, expected) => result shouldBe (expected +- 1e-3)
      }
    }
  }

  "MissingNumbers" - {
    import MissingNumbers.getMissingElements

    "should retrieve those numbers from the second list that appear less in the first list" in {
      val firstList: List[Int] = List(203, 204, 205, 206, 207, 208, 203, 204, 205, 206)
      val secondList: List[Int] = List(203, 204, 204, 205, 206, 207, 205, 208, 203, 206, 205, 206, 204)
      getMissingElements(firstList, secondList) shouldEqual List(204, 205, 206)
    }
  }

  "CommonDivisors" - {
    import CommonDivisors.calcNumberOfCommonDivisors

    "should calculate the number of common divisors of two integers" in {
      val inputs: List[List[Int]] = List(List(10, 4), List(1, 100), List(288, 240), List(85085, 453600), List(36, 27))
      inputs.map{ case List(n, m) => calcNumberOfCommonDivisors(n, m) } shouldEqual List(2, 1, 10, 4, 3)
    }
  }

  "SubsetSum" - {
    import SubsetSum.calcMinimumSubsetSizes

    "should find the size of the smallest subset whose sum is greater than or equal to the query number" in {
      val numbers: Vector[Int] = Vector(4, 8, 10, 12)
      val queries: List[Long] = List(4, 13, 30, 100)
      calcMinimumSubsetSizes(numbers, numbers.length, queries) shouldEqual List(1, 2, 3, -1)
    }
  }

  "Mangoes" - {
    import Mangoes.calcMaximalNumberOfGuests

    "should calculate the largest number of friends who can eat the available mangoes" - {
      "test case 1" in {
        val nrFriends: Int = 5
        val nrMangoes: Long = 200
        val appetite: List[Int] = List(2, 5, 3, 2, 4)
        val happiness: List[Int] = List(30, 40, 10, 20, 30)
        calcMaximalNumberOfGuests(appetite, happiness, nrFriends, nrMangoes) shouldEqual 3
      }

      "test case 2" in {
        val nrFriends: Int = 2
        val nrMangoes: Int = 100
        val appetite: List[Int] = List(3, 4)
        val happiness: List[Int] = List(1, 2)
        calcMaximalNumberOfGuests(appetite, happiness, nrFriends, nrMangoes) shouldEqual 2
      }

      "test case 3" in {
        val nrFriends: Int = 5
        val nrMangoes: Int = 100
        val appetite: List[Int] = List(100, 1000, 10000, 100000, 100)
        val happiness: List[Int] = List(100, 1000, 10000, 100000, 100)
        calcMaximalNumberOfGuests(appetite, happiness, nrFriends, nrMangoes) shouldEqual 1
      }
    }
  }

  "GameOfKayles" - {
    import GameOfKayles.doesFirstPlayerWin

    "should return if the first player wins the game of Kayles" in {
      val initialPinConfigurations: List[String] = List("IXXI", "XIIX", "IIXII", "IIIII")
      doesFirstPlayerWin(initialPinConfigurations) shouldEqual List(false, true, false, true)
    }
  }

  "RemoveDuplicates" - {
    import RemoveDuplicates.keepUniqueCharacters

    "should keep the first occurrence of each letter" in {
      val inputs: List[String] = List("aabc", "ccbabacc")
      inputs.map(keepUniqueCharacters) shouldEqual List("abc", "cba")
    }
  }

  "HugeGCD" - {
    import HugeGCD.calcGCGModulo

    "should calculate the greatest common division of two numbers modulo M given by their factors" - {
      "test case 1" in {
        val xs: List[Int] = List(2, 2, 3, 3, 25)
        val ys: List[Int] = List(8, 1, 6, 170)
        calcGCGModulo(xs, ys) shouldEqual 60
      }

      "test case 2" in {
        val xs: List[Int] = List(1, 2, 4, 8, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192)
        val ys: List[Int] = List(1, 3, 9, 27, 81, 243, 729, 2187, 6561)
        calcGCGModulo(xs, ys) shouldEqual 1
      }
    }
  }

  "CaptainPrime" - {
    import CaptainPrime.calculateIdType

    "should decide the fate of seamen based on their ids" - {
      "test case 1" in {
        val ids: List[Int] = List(3137, 1367, 2333, 101, 12)
        calculateIdType(ids) shouldEqual List("CENTRAL", "LEFT", "RIGHT", "DEAD", "DEAD")
      }

      "test case 2" in {
        val ids: List[Int] = List(43, 23, 66, 29)
        calculateIdType(ids) shouldEqual List("LEFT", "CENTRAL", "DEAD", "RIGHT")
      }
    }
  }

  "Puzzle" - {
    import Puzzle.{Point, Tromino, solvePuzzle}
    val upperLeftCorner = Point(1, 1)

    "should calculate a valid covering of a grid with trominoes" - {
      "test case 1" in {
        solvePuzzle(2, Point(2, 2), upperLeftCorner).toSet shouldEqual
          Set(Tromino(Point(1, 1), Point(1, 2), Point(2, 1)))
      }

      "test case 2" in {
        solvePuzzle(4, Point(1, 1), upperLeftCorner).toSet shouldEqual
          Set(
            Tromino(Point(2, 3), Point(3, 2), Point(3, 3)),
            Tromino(Point(1, 2), Point(2, 1), Point(2, 2)),
            Tromino(Point(1, 3), Point(1, 4), Point(2, 4)),
            Tromino(Point(3, 1), Point(4, 1), Point(4, 2)),
            Tromino(Point(3, 4), Point(4, 3), Point(4, 4))
          )
      }
    }
  }
}
