package chapter4

/**
 * A  binary tree is when the number of children is max 2
 */
object BinaryTree {
  //[1] -> [2]
  //[1] -> [3]
  //[1] is the root node with children left and right occupied, with value 2 and 3 respectively
  // with no further children. The above example is a perfect binary tree
  final case class Node[A](left: Option[Node[A]], right: Option[Node[A]], value: A)
}

object CompleteBinaryTree {
  // A complete binary tree is when either every node has either zero or two children
  // zero is Option.empty[A] and two is ((Node[A], Node[A]))
  final case class CompleteBinaryTree[A](value: A, branch: Option[(CompleteBinaryTree[A], CompleteBinaryTree[A])])
}
