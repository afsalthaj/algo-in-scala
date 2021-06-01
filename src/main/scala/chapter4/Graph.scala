package chapter4

import scala.collection.immutable
import scala.collection.immutable.Stream.Cons
import scala.collection.immutable.Stream.Empty

/**
 * Unlike a tree which can be just
 * final case class Node(a: String, children: List[Node]),
 * in Graph, you can't necessarily reach all the nodes from a single node.
 */
final case class Graph[A](nodes: List[Graph.Node[A]]) {
  // A depth first search per node in the graph without caring visited node
  // Here a definition of node is equal to the value it holds.
  def existsDFS(a1: A) = {
    def go(children: List[Graph.Node[A]]): Option[Graph.Node[A]] =
      children match {
        case head :: next =>
          head.existsDFS(a1) match {
            case Some(value) => Some(value)
            case None        => go(next)
          }
        case immutable.Nil => None
      }

    go(nodes)
  }

  // A bread first search per node in the graph
  def existsBFS(a1: A) = {
    def go(children: List[Graph.Node[A]]): Option[Graph.Node[A]] =
      children match {
        case head :: next =>
          head.existsBFS(a1) match {
            case Some(value) => Some(value)
            case None        => go(next)
          }
        case immutable.Nil => None
      }

    go(nodes)
  }
}

object Graph {
  final case class Node[A](a: A, children: List[Node[A]]) { self =>
    def existsDFS(a1: A): Option[Node[A]] =
      // pre traversal
      if (a == a1) {
        Some(self)
      } else {
        def go(list: List[Node[A]]): Option[Node[A]] =
          list match {
            case head :: next =>
              head.existsDFS(a1) match {
                case Some(value) => Some(value)
                case None        => go(next)
              }

            case immutable.Nil => None
          }

        go(children)
      }

    /**
     * Exists but with Breadth first search
     *
     * @param a1
     * @return
     */
    def existsBFS[B](a1: A): Option[Node[A]] = {
      var visited: List[A] = List.empty[A]
      val queue            = chapter3.queue.Queue.empty[Node[A]]

      if (a1 == self.a) {
        self
      } else {
        queue.enqueue(self)
      }

      def loopQueue(): Option[Graph.Node[A]] =
        queue.dequeue() match {
          case Some(value) =>
            def go(list: List[Node[A]]): Option[Node[A]] =
              list match {
                case head :: next =>
                  val headV = head.a
                  if (!visited.contains(headV)) {
                    if (headV == a1) Some(head)
                    else {
                      visited = head.a :: visited
                      queue.enqueue(head)
                      go(next) // Make sure the list is traversed and visited before calling next loopQueue(), that's the def of BFS
                    }
                  } else go(next)

                case immutable.Nil => None
              }

            go(value.children) match {
              case Some(value) => Some(value)
              case None        => loopQueue()
            }

          case None => None
        }

      loopQueue()
    }

  }
}

object GraphProblems {

  /**
   * 4.1
   * Given a directed graph, find if there is a route between two nodes
   * Many algorithms in internet doesn't consider the data type as above.
   * So may be lets find a1 from the graph. If a1 is an in 1 Graph.Node[A]
   * and a2 is in another Graph.Node[B], we don't care. a2 should be in the same Graph.Node
   * to form a connection.
   *
   * First step. Find the node in the array `nodes`, where a1 exists.
   * Then do either a DFS or BFS on that node. BFS is better IMO, as it has the tendency
   * to pick the node that match faster without going deeper.
   */
  def routeExistsDFS[A](a1: A, a2: A, graph: Graph[A]): Boolean = {
    val nodeofA1 = graph.existsBFS(a1)

    nodeofA1 match {
      case Some(node) => node.existsBFS(a2).isDefined
      case None       => false
    }
  }
}
