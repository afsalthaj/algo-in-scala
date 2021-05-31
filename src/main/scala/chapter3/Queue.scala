package chapter3

import chapter3.queue.Queue

/**
 * Mutable implementation of queue.
 * This is sort of a doubly linked list
 * where each node knows the node next and
 * the node previous.
 *
 * The nullability is encoded using Option,
 * making the implementation relatively safer.
 */
package queue {
  abstract sealed class Node[A] {
    var next: Option[Node[A]]     = None
    var previous: Option[Node[A]] = None
    val data: A
  }

  object Node {
    def apply[A](d: A) =
      new Node[A] {
        val data: A = d
      }
  }

  trait Queue[A] { self =>
    var head: Option[Node[A]] = None
    var last: Option[Node[A]] = None

    /**
     * Any queue can be co-recursed/unfolded
     * to form a lazy stream in Scala.
     * The whole idea can be used for a variety of situations in programming.
     * Example: backpressure implementation
     * using bounded-queue and streams.
     */
    def toStream: Stream[A] =
      self.dequeue() match {
        case Some(value) => Stream.cons(value, self.toStream)
        case None        => Stream.empty
      }

    def peek() =
      head.map(_.data).orElse(last.map(_.data))

    def enqueue(a: A): Queue[A] = {
      val currentNode = Node(a)

      // First insert, last is empty, and last is set to current node
      if (last.isEmpty) {
        last = Some(currentNode)
      }
      // Second insert, last is nonempty but head is empty, and head
      // is set to the new node, making sure its next is pointed to last
      // Meanwhile, previous of last is set to the new head.
      else if (head.isEmpty) {
        currentNode.next = last
        head = Some(currentNode)
        last match {
          case Some(value) =>
            value.previous = head
          case None =>
            ()
        }
      }
      // Any more insert, the new node's next is set to existing head.
      // And, existing head's previous is set to the new node.
      // Finally, head is set to the new node, so that peek() works!
      else {
        var temp = head
        currentNode.next = head
        temp.map(_.previous = Some(currentNode))
        head = Some(currentNode)
      }

      self
    }

    // Dequeue, implies, returning existing last's data,
    // and assigning last to it's previous node.
    def dequeue() = {
      var temp = last
      last match {
        case Some(value) =>
          last = value.previous
        case None => ()
      }

      temp.map(_.data)
    }

    def isEmpty  = peek().isDefined
    def nonEmpty = !isEmpty
  }

  object Queue {
    def empty[A] = new Queue[A] {}

    // List(1, 2, 3) ==> Queue(1, 2, 3) ==> 3 is the last element,
    // implies 3 is inserted first
    def fromList[A](list: List[A]): Queue[A] =
      list.foldRight(Queue.empty[A])((a, b) => b.enqueue(a))
  }
}

object QueueApp extends App {
  val queue = new Queue[Int] {}
  queue.enqueue(1)
  assert(queue.peek() == Some(1))
  queue.enqueue(2)
  assert(queue.peek() == Some(2))
  queue.enqueue(3)
  assert(queue.peek() == Some(3))
  queue.enqueue(4)
  assert(queue.peek() == Some(4))
  queue.enqueue(5)
  assert(queue.peek() == Some(5))

  assert(queue.dequeue() == Some(1))
  assert(queue.dequeue() == Some(2))
  assert(queue.dequeue() == Some(3))
  assert(queue.dequeue() == Some(4))
  assert(queue.dequeue() == Some(5))
  assert(Queue.fromList(List(1, 2, 3)).dequeue() == Some(3))
  assert(Queue.fromList((0 to 10000).toList).toStream.take(5).toList == List(10000, 9999, 9998, 9997, 9996))
}
