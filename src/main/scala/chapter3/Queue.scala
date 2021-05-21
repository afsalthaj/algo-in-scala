package chapter3
import chapter3.Queue.EmptyQueue
import chapter3.Queue.ConsQueue

/**
 * A stack unsafe queue, however constant in time
 * enqueue and dequeue.
 */
sealed trait Queue[A] { self =>
  def dequeue(): (Option[A], Queue[A]) = self match {
    case EmptyQueue()          => (None, self)
    case Queue.Singleton(a)    => (Some(a), EmptyQueue())
    case ConsQueue(tail, last) => (Some(last), tail)
  }

  def enqueue(list: List[A]): Queue[A] =
    list.foldRight(self)((a, b) => b.enqueue(a))

  def enqueue(elem: A): Queue[A] = self match {
    case c @ EmptyQueue() =>
      ConsQueue(EmptyQueue(), elem)

    case c @ Queue.Singleton(v) =>
      ConsQueue(Queue.Singleton(elem), c.a)

    case c @ ConsQueue(EmptyQueue(), last) =>
      ConsQueue(Queue.Singleton(elem), last)

    case c @ ConsQueue(queue, last) =>
      val (l: Option[A], newQueue: Queue[A]) = queue.dequeue()

      l match {
        case Some(value) => ConsQueue(ConsQueue(newQueue.enqueue(elem), value), last)
        case None        => ConsQueue(newQueue.enqueue(elem), last)
      }
  }
}

object Queue {
  def init[A] = EmptyQueue[A]()
  final case class EmptyQueue[A]()                       extends Queue[A]
  final case class Singleton[A](a: A)                    extends Queue[A]
  final case class ConsQueue[A](tail: Queue[A], last: A) extends Queue[A]
}

object QueueExample extends App {
  val queue                  = Queue.init[Int]
  val nextQueue              = queue.enqueue(1)
  val nextQueue2             = nextQueue.enqueue(2)
  val nextQueue3: Queue[Int] = nextQueue2.enqueue(3)
  val nextQueue4: Queue[Int] = nextQueue3.enqueue(4)
  val nextQueue5: Queue[Int] = nextQueue4.enqueue(5)
  val nextQueue6: Queue[Int] = nextQueue5.enqueue(6)

  import Queue._

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
}
