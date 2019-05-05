package functional_structures

import org.scalatest.{FreeSpec, Matchers}

class FunctionalStructuresSuite extends FreeSpec with Matchers {

  "SwapNodes" - {
    import SwapNodes.{BinaryTree, buildBinaryTree, performTraversals}

    "should swap the subtrees of a binary tree at depth k, 2k, ... etc" - {
      "test case 1" in {
        val nrNodes: Int = 3
        val childIndices: Iterator[String] = Iterator("2 3", "-1 -1", "-1 -1")
        val swaps: List[Int] = List(1, 1)
        val tree: BinaryTree = buildBinaryTree(childIndices, nrNodes)
        val traversals: List[List[Int]] = performTraversals(tree, swaps)
        traversals shouldEqual List(List(3, 1, 2), List(2, 1, 3))
      }

      "test case 2" in {
        val nrNodes: Int = 5
        val childIndices: Iterator[String] = Iterator("2 3", "-1 4", "-1 5", "-1 -1", "-1 -1")
        val swaps: List[Int] = List(2)
        val tree: BinaryTree = buildBinaryTree(childIndices, nrNodes)
        val traversals: List[List[Int]] = performTraversals(tree, swaps)
        traversals shouldEqual List(List(4, 2, 1, 5, 3))
      }

      "test case 3" in {
        val nrNodes: Int = 11
        val childIndices: Iterator[String] =
          Iterator("2 3", "4 -1", "5 -1", "6 -1", "7 8", "-1 9", "-1 -1", "10 11", "-1 -1", "-1 -1", "-1 -1")
        val swaps: List[Int] = List(2, 4)
        val tree: BinaryTree = buildBinaryTree(childIndices, nrNodes)
        val traversals: List[List[Int]] = performTraversals(tree, swaps)
        traversals shouldEqual List(List(2, 9, 6, 4, 1, 3, 7, 5, 11, 8, 10), List(2, 6, 9, 4, 1, 3, 7, 5, 10, 8, 11))
      }
    }
  }

  "ValidBinarySearchTree" - {
    import ValidBST.isValidBST

    "should determine if a given list is a preorder traversal of a valid binary search tree" in {
      val input: List[List[Int]] =
        List(
          List(1, 2, 3),
          List(2, 1, 3),
          List(3, 2, 1, 5, 4, 6),
          List(1, 3, 4, 2),
          List(3, 4, 5, 1, 2)
        )
      input.map(isValidBST) shouldEqual List(true, true, true, false, false)
    }
  }

  "ListAndGCD" - {
    import functional_structures.ListAndGCD.{CanonicalForm, readCanonicalForm, calcGCD}

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
    import functional_structures.SubstringSearching.{TextData, doesPatternOccurAsSubstring}

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

  "MatrixRotation" - {
    import functional_structures.MatrixRotation.{Matrix, calcRotatedMatrix}

    "should rotate the matrices in counterclockwise direction" - {
      "test case 1" in {
        val nRows: Int = 4
        val nCols: Int = 4
        val r: Int = 1
        val matrix: Matrix =
          Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12), Vector(13, 14, 15, 16))
        calcRotatedMatrix(matrix, nRows, nCols, r) shouldEqual
          Vector(Vector(2, 3, 4, 8), Vector(1, 7, 11, 12), Vector(5, 6, 10, 16), Vector(9, 13, 14, 15))
      }

      "test case 2" in {
        val nRows: Int = 4
        val nCols: Int = 4
        val r: Int = 2
        val matrix: Matrix =
          Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12), Vector(13, 14, 15, 16))
        calcRotatedMatrix(matrix, nRows, nCols, r) shouldEqual
          Vector(Vector(3, 4, 8, 12), Vector(2, 11, 10, 16), Vector(1, 7, 6, 15), Vector(5, 9, 13, 14))
      }

      "test case 3" in {
        val nRows: Int = 5
        val nCols: Int = 4
        val r: Int = 7
        val matrix: Matrix =
          Vector(
            Vector(1, 2, 3, 4),
            Vector(7, 8, 9, 10),
            Vector(13, 14, 15, 16),
            Vector(19, 20, 21, 22),
            Vector(25, 26, 27, 28)
          )
        calcRotatedMatrix(matrix, nRows, nCols, r) shouldEqual Vector(Vector(28, 27, 26, 25), Vector(22, 9, 15, 19),
            Vector(16, 8, 21, 13), Vector(10, 14, 20, 7), Vector(4, 3, 2, 1))
      }

      "test case 4" in {
        val nRows: Int = 2
        val nCols: Int = 2
        val r: Int = 3
        val matrix: Matrix = Vector(Vector(1, 1), Vector(1, 1))
        calcRotatedMatrix(matrix, nRows, nCols, r) shouldEqual Vector(Vector(1, 1), Vector(1, 1))
      }
    }
  }

  "TreeManager" - {
    import TreeManager.{Operation, performTreeOperations, readOperations}

    "should handle all the tree operations" in {
      val nrOperations: Int = 11
      val input: Iterator[String] =
        List(
          "change 1",
          "print",
          "insert child 2",
          "visit child 1",
          "insert right 3",
          "visit right",
          "print",
          "insert right 4",
          "delete",
          "visit child 2",
          "print"
      ).toIterator
      val treeOperations: List[Operation] = readOperations(input, nrOperations)
      performTreeOperations(treeOperations) shouldEqual List(1, 3, 4)
    }
  }

  "PrisonTransport" - {
    import PrisonTransport.calcMinimalTransportationCost

    "should calculate the minimum cost of transporting prisoners" - {
      "test case 1" in {
        val nrPrisoners: Int = 4
        val handcuffedPairs: List[(Int, Int)] = List((0, 1), (0, 3))
        calcMinimalTransportationCost(nrPrisoners, handcuffedPairs) shouldEqual 3
      }

      "test case 2" in {
        val nrPrisoners: Int = 10
        val handcuffedPairs: List[(Int, Int)] = List((0, 9), (9, 2), (1, 2), (3, 4), (3, 6), (3, 7), (6, 8), (4, 8))
        calcMinimalTransportationCost(nrPrisoners, handcuffedPairs) shouldEqual 6
      }
    }
  }

  "FightingArmies" - {
    import FightingArmies.manageArmies

    "should retrieve the maximum combat ability for any given armies" in {
      val nrArmies: Int = 2
      val events: Iterator[String] = List("3 1 10", "3 2 20", "4 1 2", "1 1", "2 1", "1 1").toIterator
      manageArmies(events, nrArmies) shouldEqual List(20, 10)
    }
  }

  "JohnAndFences" - {
    import JohnAndFences.calcLargestRectangle

    "should calculate the area of the largest rectangle under a histogram" - {
      "test case 1" in {
        val nrFences: Int = 6
        val heights: List[Int] = List(2, 5, 7, 4, 1, 8)
        calcLargestRectangle(heights, nrFences) shouldEqual 12
      }

      "test case 2" in {
        val nrFences: Int = 3
        val heights: List[Int] = List(2, 3, 4)
        calcLargestRectangle(heights, nrFences) shouldEqual 6
      }

      "test case 3" in {
        val nrFences: Int = 1
        val heights: List[Int] = List(5)
        calcLargestRectangle(heights, nrFences) shouldEqual 5
      }

      "test case 4" in {
        val nrFences: Int = 3
        val heights: List[Int] = List(4, 3, 2)
        calcLargestRectangle(heights, nrFences) shouldEqual 6
      }

      "test case 5" in {
        val nrFences: Int = 5
        val heights: List[Int] = List(2, 3, 8, 7, 3)
        calcLargestRectangle(heights, nrFences) shouldEqual 14
      }

      "test case 6" in {
        val nrFences: Int = 5
        val heights: List[Int] = List(2, 3, 5, 4, 3)
        calcLargestRectangle(heights, nrFences) shouldEqual 12
      }
    }
  }
}
