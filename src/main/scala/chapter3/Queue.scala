package chapter3
import chapter3.Queue.EmptyQueue
import chapter3.Queue.ConsQueue

/**
 * An immutable queue, with constant in time dequeue and enqueue operation
 * performing LIFO.
 *
 * The mutable queue implementation is
 * complicated (or hard to follow) in the text book, and is prone to errors.
 *
 * Here we prefer an immutable queue, and the trade off is a bit of
 * space complexity, however this is nominal, as the mutable implementations
 * does involve creation of a space called "QueueNode" everytime we push!
 *
 * The boxing ops are relatively higher.
 * Conceptually that would be as follows
 *
 * Given a queue: SpaceX = Head(Space(A)) -> Middle(Space(B) -> Space(C)) -> Last(Space(D))
 * Adding a new element K to SpaceX implies, we create a new SpaceY. ** implies reference.
 * SpaceY = NewHead(K) -> NewMiddle**(SpaceX.Head -> SpaceX.Middle) -> NewSpace**(SpaceX.Last))
 * Adding another new element L
 * NewHead(L) -> NewMiddle**(SpaceY.Head ->SpaceY.Middle) -> NewSpace**(SpaceY.Last))
 *
 * Apparently this implies, the box operations are fairly minimal, and you should be good to go
 * with most of the algorithms.
 */
sealed trait Queue[A] { self =>
  val lastElement = self match {
    case ConsQueue(head, tail, last) => if (tail.isEmpty && last.isEmpty) Some(head) else last
    case EmptyQueue()                => None
  }

  // Definition of middleElements is as follows
  // 1, 2, 3 ==> Middle is 2, and lastElement is 3
  // 1, 2, None => Middle is 1 (this is invalid, you can replace this case with throwing an exception)
  // 1, Empty, None => No middle element (because there is only lastElement)
  // 1, Empty, 3 ==> Middle element is actually 1 (and lastElement is 3)
  def middleElements: Queue[A] = self match {
    case a @ ConsQueue(head, m, last) =>
      (m, last) match {
        case (EmptyQueue(), None)              => EmptyQueue()
        case (s @ ConsQueue(_, _, _), None)    => s
        case (EmptyQueue(), Some(a))           => ConsQueue(head, EmptyQueue(), None)
        case (s @ ConsQueue(_, _, _), Some(a)) => s
      }
    case EmptyQueue()                 => EmptyQueue()
  }

  def isEmpty: Boolean = self match {
    case EmptyQueue()                => true
    case ConsQueue(head, tail, last) => false
  }

  def dequeue(): Queue[A] = self match {
    case EmptyQueue()                    => self
    case c @ ConsQueue(h1, middle, last) =>
      // if cons and last is empty, it implies 1 element which is head, and we remove it!
      if (middle.isEmpty && last.isEmpty) {
        EmptyQueue()
      } else {
        // Else we remove the last, and shift the last of middle (cons.lastElement) to last.
        // and keep the rest of the elements in the middle (cons.middleElements) in the middle
        ConsQueue(h1, middle.middleElements, middle.lastElement)
      }

  }

  def enqueue(elem: A) = self match {
    // Push 1 element to zero element queue.
    // Middle is Empty. Last is empty. Head exists.
    case c @ EmptyQueue()                  =>
      ConsQueue(elem, EmptyQueue(), None)

    // Push 1 element to 1 element queue.
    // Middle is still empty. Head and Last exists.
    case c @ ConsQueue(head, middle, None) =>
      ConsQueue(elem, EmptyQueue(), Some(head))

    // Push 1 element to mulitple elements queue.
    case c @ ConsQueue(head, cons, last)   =>
      ConsQueue(elem, ConsQueue(head, cons.middleElements, cons.lastElement), last)
  }

}

object Queue {
  def init[A] = EmptyQueue[A]()
  final case class EmptyQueue[A]()                                        extends Queue[A]
  final case class ConsQueue[A](head: A, tail: Queue[A], last: Option[A]) extends Queue[A]
}

object QueueExample extends App {
  val queue                  = Queue.init[Int]
  val nextQueue              = queue.enqueue(1)
  val nextQueue2             = nextQueue.enqueue(2)
  val nextQueue3: Queue[Int] = nextQueue2.enqueue(3)
  val nextQueue4: Queue[Int] = nextQueue3.enqueue(4)

  assert(nextQueue4 == ConsQueue(4, ConsQueue(3, EmptyQueue(), Some(2)), Some(1)))
  assert(nextQueue4.dequeue() == ConsQueue(4, ConsQueue(3, EmptyQueue(), None), Some(2)))
  assert(nextQueue4.dequeue().dequeue() == ConsQueue(4, EmptyQueue(), Some(3)))
  assert(nextQueue4.dequeue().dequeue().dequeue() == ConsQueue(4, EmptyQueue(), None))
  assert(nextQueue4.dequeue().dequeue().dequeue().dequeue == EmptyQueue())
}
