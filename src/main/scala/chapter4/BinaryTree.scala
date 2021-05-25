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
