package adhoc

import org.scalatest.{FreeSpec, Matchers, Inspectors}

class AdhocSuite extends FreeSpec with Matchers with Inspectors {

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
}
