package chapter4

/**
 * A  binary tree is when the number of children is max 2
 */
object BinaryTree {
  //[1] -> [2]
  //[1] -> [3]
  //[1] is the root node with children left and right occupied, with vlue 2 and 3 respectively
  // with no further children. The above example is a perfect binary tree
  final case class Node[A](left: Option[Node[A]], right: Option[Node[A]], value: A)
}

object CompleteBinaryTree {
  // A complete binary tree is when either every node has either zero or two children
  // zero is Option.empty[A] and two is ((Node[A], Node[A]))
  final case class CompleteBinaryTree[A](value: A, branch: Option[(CompleteBinaryTree[A], CompleteBinaryTree[A])])
}

object Traversals {
  // Pretraversal. visit current >> Node left >> Node right
  // In traversal. Node left >> visit current >> Node right
  // Post traversal. Node left >> Node right >> visit current
  // This is just a compilable pseudo-code. Don't use it anywhere. A => Unit
  // is a bad practice.
  def inTraversal[A, B](tree: Option[BinaryTree.Node[A]], f: A => Unit): Unit =
    tree match {
      case Some(value) =>
        inTraversal(value.left, f)
        f(value.value)
        inTraversal(value.right, f)
      case None        =>
        ()
    }

  def preTraversal[A, B](tree: Option[BinaryTree.Node[A]], f: A => Unit): Unit =
    tree match {
      case Some(value) =>
        f(value.value)
        inTraversal(value.left, f)
        inTraversal(value.right, f)
      case None        =>
        ()
    }

  def postTraversal[A, B](tree: Option[BinaryTree.Node[A]], f: A => Unit): Unit =
    tree match {
      case Some(value) =>
        inTraversal(value.left, f)
        inTraversal(value.right, f)
        f(value.value)
      case None        =>
        ()
    }
}

object TreeProblems extends App {

  /**
   * 4.2 (4.1 is a graph problem, look for Graph.scala)
   * Minimal tree: Given a sorted array with unique integer elements
   * write an algorithm to create a binary tree with minimal height.
   *
   * The minimal height means minimum depth. To keep the depth less,
   * make sure the root node is middle of the array. Example:
   * 1, 2, 3, 4. If you pick 1 as the root node of the binary tree,
   * then the depth is 4. 1 -right-> 2 -right-> 3 -right-> 4
   *
   * FIXME: Handle empty array
   */
  // 1 2 3 4 5
  def minimalTree(array: Array[Int]): BinaryTree.Node[Int] = {
    def minimalTree(array: Array[Int], start: Int, end: Int): BinaryTree.Node[Int] = {
      val middle = start + end / 2

      if (start + 1 == end) {
        BinaryTree.Node(Some(BinaryTree.Node(None, None, array(start))), None, array(end))
      } else if (start == end) {
        BinaryTree.Node(None, None, array(end))
      } else {
        val newLeft0 = start
        val newLeft1 = middle - 1

        val newRight0 = middle + 1
        val newRight1 = end
        BinaryTree.Node(Some(minimalTree(array, newLeft0, newLeft1)), Some(minimalTree(array, newRight0, newRight1)), middle)
      }
    }

    minimalTree(array, 0, array.length - 1)
  }

  /**
   * 4.3 Listof depths
   * Given a binary tree, design an algoritm which creates a linked list of all the nodes at each
   * depth (eg. if you have a tree with depth D, you will have D linked list)
   *
   * FIXME: Slightly complicated. Clean up
   */
  def listOfDepths[A](root: BinaryTree.Node[A]): List[List[A]] = {
    val queue: chapter3.queue.Queue[BinaryTree.Node[A]] = chapter3.queue.Queue.empty[BinaryTree.Node[A]]
    queue.enqueue(root)
    var nodesOnEachLevel                                = 0
    var nodesOnEachLevelTmp                             = 1
    var totalList: List[List[A]]                        = Nil
    var listOnEachLevel: List[A]                        = Nil

    while (queue.nonEmpty) {
      nodesOnEachLevel = nodesOnEachLevelTmp
      nodesOnEachLevelTmp = 0 // Just acting as temporary (it's equivalent to saying nodesOnEachlevel = queue.size)
      listOnEachLevel = Nil
      while (nodesOnEachLevel > 0) {
        val node = queue.dequeue()

        node match {
          case Some(node) =>
            listOnEachLevel = node.value :: listOnEachLevel

            node.left match {
              case Some(value) =>
                queue.enqueue(value)
                nodesOnEachLevelTmp = nodesOnEachLevelTmp + 1
              case None        => ()
            }

            node.right match {
              case Some(value) =>
                queue.enqueue(value)
                nodesOnEachLevelTmp = nodesOnEachLevelTmp + 1
              case None        => ()
            }

          case None => ()
        }

        nodesOnEachLevel = nodesOnEachLevel - 1
      }
      totalList = listOnEachLevel :: totalList
    }

    totalList
  }

  /**
   * 4.4 Check balanced. Implement a function to check if binary tree is balanced. For the purposes of
   * this question, a balanced tree is defined to be a tree such that the heights of two subtrees of any node
   * never differe by more than one. S
   *
   * Ans: Similar to the above logic of BFS
   * FIXME: To be cleaned up
   */
  type UnbalancedFlag = Int // If 0, no action, if 1 keep adding
  def checkIfBinaryTreeIsBalanced[A](root: BinaryTree.Node[A]): Boolean = {
    val queue: chapter3.queue.Queue[(BinaryTree.Node[A], UnbalancedFlag)] = chapter3.queue.Queue.empty[(BinaryTree.Node[A], UnbalancedFlag)]
    if (root.left.isDefined && root.right.isDefined || root.left.isEmpty && root.right.isEmpty) {
      queue.enqueue((root, 0))
    } else {
      queue.enqueue((root, 1))
    }
    var nodesOnEachLevel                                                  = 0
    var nodesOnEachLevelTmp                                               = 1

    var unbalanced: Boolean = false

    while (queue.nonEmpty) {
      nodesOnEachLevel = nodesOnEachLevelTmp
      nodesOnEachLevelTmp = 0 // Just acting as temporary (it's equivalent to saying nodesOnEachlevel = queue.size)
      while (nodesOnEachLevel > 0) {
        val node = queue.dequeue()

        node match {
          case Some(nodeInt) =>
            val (node, int) = nodeInt

            (node.left, node.right) match {
              case (Some(nodeL), Some(nodeR)) =>
                if (int == 1) {
                  unbalanced = true
                } else {
                  queue.enqueue((nodeL, 0))
                  queue.enqueue((nodeR, 0))
                  nodesOnEachLevelTmp = nodesOnEachLevelTmp + 2
                }

              case (Some(nodeL), None) =>
                if (int == 1) {
                  unbalanced = true
                } else {
                  queue.enqueue((nodeL, 1))
                  nodesOnEachLevelTmp = nodesOnEachLevelTmp + 1
                }

              case (None, Some(nodeR)) =>
                if (int == 1) {
                  unbalanced = true
                } else {
                  queue.enqueue((nodeR, 1))
                  nodesOnEachLevelTmp = nodesOnEachLevelTmp + 1
                }
              case (None, None)        => ()
            }

          case None => ()
        }

        nodesOnEachLevel = nodesOnEachLevel - 1
      }
    }

    !unbalanced
  }

}
