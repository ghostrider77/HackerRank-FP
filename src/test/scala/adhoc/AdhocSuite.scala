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

  "ListAndGCD" - {
    import ListAndGCD.{CanonicalForm, readCanonicalForm, calcGCD}

    "should calculate the greatest common divisor of elements of a list given in their canonical forms" - {
      "test case 1" in {
        val n: Int = 2
        val lines: List[String] = List("7 2", "2 2 7 1")
        val numbers: List[CanonicalForm] = lines.map(readCanonicalForm)
        calcGCD(numbers) shouldEqual Map(7 -> 1)
      }

      "test case 2" in {
        val n: Int = 4
        val lines: List[String] = List("2 2 3 2 5 3", "3 2 5 3 11 1", "2 2 3 3 5 4 7 6 19 18", "3 10 5 15")
        val numbers: List[CanonicalForm] = lines.map(readCanonicalForm)
        calcGCD(numbers) shouldEqual Map(3 -> 2, 5 -> 3)
      }
    }
  }

  "SubstringSearching" - {
    import SubstringSearching.{TextData, doesPatternOccurAsSubstring}

    "should detect if pattern appears as substring in the given text" in {
      val input: List[TextData] =
        List(
          TextData(text = "abcdef", pattern = "def"),
          TextData(text = "computer", pattern = "muter"),
          TextData(text = "stringmatchingmat", pattern = "ingmat"),
          TextData(text = "videobox", pattern = "videobox")
        )
      input.map(doesPatternOccurAsSubstring) shouldEqual List(true, false, true, true)
    }
  }
}
