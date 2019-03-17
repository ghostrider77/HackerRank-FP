package adhoc

import org.scalatest.{FreeSpec, Matchers}

class AdhocSuite extends FreeSpec with Matchers {

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
