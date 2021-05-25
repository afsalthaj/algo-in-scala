package chapter4

/**
 * Unlike a tree which can be just
 * final case class Node(a: String, children: List[Node]),
 * in Graph, you can't necessarily reach all the nodes from a single node.
 */
final case class Graph(node: Array[Graph.Node])

object Graph {
  final case class Node(a: String, children: List[Node])
}
