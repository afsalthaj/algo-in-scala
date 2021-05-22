package chapter3
import chapter3.QueueUnsafe.EmptyQueue
import chapter3.QueueUnsafe.ConsQueue

/**
 * This is a stack unsafe immutable queue, with constant in time
 * enqueue and dequeue.
 *
 * There are numerous
 * safe implementations of immutable queue (Ex: Refer Scala),
 * however, we are taking a different approach here.
 *
 * The restructuring during enqueue is done
 * through recursive dequeue and enqueue, however
 * it is stack unsafe. The whole purpose is fun and learning.
 * Compare the implementation with mutable queue in `chapter3.queue.Queue`.
 */
sealed trait QueueUnsafe[A] { self =>
  def dequeue(): (Option[A], QueueUnsafe[A]) = self match {
    case EmptyQueue()             => (None, self)
    case QueueUnsafe.Singleton(a) => (Some(a), EmptyQueue())
    case ConsQueue(tail, last)    => (Some(last), tail)
  }

  def enqueue(list: List[A]): QueueUnsafe[A] =
    list.foldRight(self)((a, b) => b.enqueue(a))

  def enqueue(elem: A): QueueUnsafe[A] = self match {
    case c @ EmptyQueue() =>
      ConsQueue(EmptyQueue(), elem)

    case c @ QueueUnsafe.Singleton(v) =>
      ConsQueue(QueueUnsafe.Singleton(elem), c.a)

    case c @ ConsQueue(EmptyQueue(), last) =>
      ConsQueue(QueueUnsafe.Singleton(elem), last)

    case c @ ConsQueue(queue, last) =>
      val (l: Option[A], newQueue: QueueUnsafe[A]) = queue.dequeue()
      val enqueued                                 = newQueue.enqueue(elem)

      l match {
        case Some(value) => ConsQueue(ConsQueue(enqueued, value), last)
        case None        => ConsQueue(enqueued, last)
      }
  }
}

object QueueUnsafe {
  def init[A] = EmptyQueue[A]()
  final case class EmptyQueue[A]()                             extends QueueUnsafe[A]
  final case class Singleton[A](a: A)                          extends QueueUnsafe[A]
  final case class ConsQueue[A](tail: QueueUnsafe[A], last: A) extends QueueUnsafe[A]
}

object QueueExample extends App {
  val queue                        = QueueUnsafe.init[Int]
  val nextQueue                    = queue.enqueue(1)
  val nextQueue2                   = nextQueue.enqueue(2)
  val nextQueue3: QueueUnsafe[Int] = nextQueue2.enqueue(3)
  val nextQueue4: QueueUnsafe[Int] = nextQueue3.enqueue(4)
  val nextQueue5: QueueUnsafe[Int] = nextQueue4.enqueue(5)
  val nextQueue6: QueueUnsafe[Int] = nextQueue5.enqueue(6)

  import QueueUnsafe._

  val (l0, n0) = nextQueue6.dequeue()
  assert(l0 == Some(1))
  assert(n0 == ConsQueue(ConsQueue(ConsQueue(ConsQueue(Singleton(6), 5), 4), 3), 2))
  val (l1, n1) = n0.dequeue()
  assert(l1 == Some(2))
  val (l2, n2) = n1.dequeue()
  assert(l2 == Some(3))
  val (l3, n3) = n2.dequeue()
  assert(l3 == Some(4))
  val (l4, n4) = n3.dequeue()
  assert(l4 == Some(5))
  assert(n4 == Singleton(6))

  val hugeQueue = queue.enqueue(List.fill(10)(1))
}
