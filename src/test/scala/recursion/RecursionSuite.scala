package recursion

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inspectors

class RecursionSuite extends AnyFreeSpec with Matchers with Inspectors {
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

  "StringCompression" - {
    import StringCompression.compress

    "should compress a string" in {
      val input: List[String] = List("abcaaabbb", "abcd", "aaabaaaaccaaaaba")
      input.map(compress) shouldEqual List("abca3b3", "abcd", "a3ba4c2a4ba")
    }
  }

  "PrefixCompression" - {
    import PrefixCompression.{Result, findCommonPrefix}

    "should find the common prefix of 2 strings" in {
      val Result(p, suff1, suff2) = findCommonPrefix("abcdefpr".toList, "abcpqr".toList)
      p shouldEqual "abc"
      suff1 shouldEqual "defpr"
      suff2 shouldEqual "pqr"
    }
  }

  "StringReduction" - {
    import StringReductions.keepUniqueCharacters

    "should keep those characters that do not occured previously in a string" in {
      val inputs: List[String] = List("accabb", "abc", "pprrqq", "abcaaaaaaaaaaabbbbbbbcccccccd")
      inputs.map(keepUniqueCharacters) shouldEqual List("acb", "abc", "prq", "abcd")
    }
  }

  "SuperQueens" - {
    import SuperQueens.calcNumberOfWays

    "should calculate the number of ways to place n super-queens onto a chessboard of size n" in {
      calcNumberOfWays(10) shouldEqual 4
    }
  }

  "SumOfPowers" - {
    import SumOfPowers.sumOfPowers

    "should calculate the number of ways that an integer is the sum of the power of unique natural numbers" in {
      sumOfPowers(10, 2) shouldEqual 1
      sumOfPowers(100, 2) shouldEqual 3
      sumOfPowers(100, 3) shouldEqual 1
    }
  }

  "SequenceFullOfColors" - {
    import SequenceFullOfColors.hasFullColors

    "should check if a sequence of colors satisfies some conditions" in {
      val inputs: List[String] = List("RGGR", "RYBG", "RYRB", "YGYGRBRB")
      inputs.map(hasFullColors) shouldEqual List(true, true, false, false)
    }
  }

  "FilterElements" - {
    import FilterElements.{TestCase, filterElements}

    "should return those elements in the order of their first appearance that occurs at least k times" in {
      val input: List[TestCase] =
        List(
          TestCase(sequence = List(4, 5, 2, 5, 4, 3, 1, 3, 4), k = 2),
          TestCase(sequence = List(4, 5, 2, 5, 4, 3, 1, 3, 4), k = 4),
          TestCase(sequence = List(5, 4, 3, 2, 1, 1, 2, 3, 4, 5), k = 2),
        )
      input.map(filterElements) shouldEqual List(List(4, 5, 3), List(-1), List(5, 4, 3, 2, 1))
    }
  }

  "SuperDigit" - {
    import SuperDigit.calcCompoundSuperDigit

    "should compute the super-digit of an integer concatenated n times" in {
      calcCompoundSuperDigit("148", 3) shouldEqual 3
    }
  }

  "ConcavePolygon" - {
    import ConcavePolygon.{Point, isPolygonConcave}

    "should determine whether a polygon given by its vertices is concave" - {
      "test case 1" in {
        val points: List[Point] = List(Point(0, 0), Point(0, 1), Point(1, 1), Point(1, 0))
        isPolygonConcave(points) shouldBe false
      }

      "test case 2" in {
        val points: List[Point] = List(Point(0, 0), Point(2, 0), Point(0, 2), Point(2, 2), Point(1, 0))
        isPolygonConcave(points) shouldBe false
      }

      "test case 3" in {
        val points: List[Point] = List(Point(0, 0), Point(2, 0), Point(3, -1), Point(4, 2), Point(2, 2), Point(1, 1))
        isPolygonConcave(points) shouldBe true
      }

      "test case 4" in {
        val points: List[Point] = List(Point(4, 0), Point(2, 1), Point(3, -1), Point(2, 0), Point(1, -1), Point(0, 0))
        isPolygonConcave(points) shouldBe true
      }
    }
  }

  "Crosswords-101" - {
    import Crosswords101.{Board, solveCrossword, readBoard}

    "should solve a crossword" - {
      "test case 1" in {
        val lines: Iterator[String] =
          Iterator(
            "+-++++++++",
            "+-++++++++",
            "+-++++++++",
            "+-----++++",
            "+-+++-++++",
            "+-+++-++++",
            "+++++-++++",
            "++------++",
            "+++++-++++",
            "+++++-++++"
          )
        val board: Board = readBoard(lines)
        val words: List[String] = List("LONDON", "DELHI", "ICELAND", "ANKARA")
        val solution: Iterator[String] =
          Iterator(
            "+L++++++++",
            "+O++++++++",
            "+N++++++++",
            "+DELHI++++",
            "+O+++C++++",
            "+N+++E++++",
            "+++++L++++",
            "++ANKARA++",
            "+++++N++++",
            "+++++D++++"
          )
        val solutionBoard: Board = readBoard(solution)
        solveCrossword(board, words) should contain (solutionBoard)
      }

      "test case 2" in {
        val lines: Iterator[String] =
          Iterator(
            "+-++++++++",
            "+-++++++++",
            "+-------++",
            "+-++++++++",
            "+-++++++++",
            "+------+++",
            "+-+++-++++",
            "+++++-++++",
            "+++++-++++",
            "++++++++++"
          )
        val board: Board = readBoard(lines)
        val words: List[String] = List("AGRA", "NORWAY", "ENGLAND", "GWALIOR")
        val solution: Iterator[String] =
          Iterator(
            "+E++++++++",
            "+N++++++++",
            "+GWALIOR++",
            "+L++++++++",
            "+A++++++++",
            "+NORWAY+++",
            "+D+++G++++",
            "+++++R++++",
            "+++++A++++",
            "++++++++++"
          )
        val solutionBoard: Board = readBoard(solution)
        solveCrossword(board, words) should contain (solutionBoard)
      }
    }
  }

  "Tree Of Life" - {
    import TreeOfLife.{BinaryTree, CellState, On, Off, runSimulation, buildTree}

    "should print the state of the machine at a point reachable from a given path" - {
      val treeString: String = "((. X (. . .)) . (X . (. X X)))"
      val rule: Int = 42354

      "test case 1" in {
        val tree: BinaryTree = buildTree(treeString.toList)
        val paths: List[(Int, List[Char])] = List((0, Nil), (0, List('>', '<')), (0, List('>', '>', '>')))
        val result: List[CellState] = runSimulation(tree, rule, paths)
        result shouldEqual List(Off, On, On)
      }

      "test case 2" in {
        val tree: BinaryTree = buildTree(treeString.toList)
        val paths: List[(Int, List[Char])] = List((0, Nil), (1, Nil), (1, Nil), (1, Nil), (1, Nil))
        val result: List[CellState] = runSimulation(tree, rule, paths)
        result shouldEqual List(Off, On, Off, On, Off)
      }

      "test case 3" in {
        val tree: BinaryTree = buildTree(treeString.toList)
        val paths: List[(Int, List[Char])] = List((4, Nil), (0, Nil))
        val result: List[CellState] = runSimulation(tree, rule, paths)
        result shouldEqual List(Off, Off)
      }

      "test case 4" in {
        val tree: BinaryTree = buildTree(treeString.toList)
        val paths: List[(Int, List[Char])] =
          List((0, List('<', '>')), (4, List('>', '>', '<')), (-3, Nil), (1, List('>', '>', '<')))
        val result: List[CellState] = runSimulation(tree, rule, paths)
        result shouldEqual List(Off, On, On, Off)
      }
    }
  }
}
