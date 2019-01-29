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
}
