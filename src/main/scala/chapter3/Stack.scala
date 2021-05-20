package chapter3

/**
 * Stack is LIFO, unlike Queue which is FIFO
 * So pop() is always a constant operation, as we always
 * have control over the head.
 *
 * This implies Stack could be implemented in terms of Chapter2.LinkedList,
 * however, someone should be able to copy this Class file and paste it anywhere
 * and user, and hence it is intentionally made as independent
 * as possible.
 */
sealed trait Stack[A] { self =>

  /**
   * pop returns the head of the stack,
   * and also mutates the stack for
   * easiness in implementing certain algorithms.
   * This doesn't necessarily mean all operations
   * are mutations.
   */
  def pop(): Option[A] = self match {
    case EmptyStack()              => None
    case a @ StackCons(head, next) =>
      next match {
        case EmptyStack()                         => None
        case tail @ StackCons(nextHead, nextTail) =>
          a.head = nextHead
          a.next = nextTail
          Some(head)
      }
  }

  /**
   * peek() doesn't change the stack
   */
  def peek(): Option[A] = self match {
    case EmptyStack()       => None
    case StackCons(head, _) => Some(head)
  }

  /**
   * push() doesn't change the stack
   * however returns a new stack with head element
   */
  def push(a: A): Stack[A] = self match {
    case e @ EmptyStack()                => StackCons(a, e)
    case current @ StackCons(head, next) => StackCons(a, current)
  }

  /**
   * Checks if a stack is empty or not.
   */
  def isEmpty(): Boolean = self match {
    case EmptyStack()    => true
    case StackCons(_, _) => false
  }
}

final case class EmptyStack[A]()                               extends Stack[A]
final case class StackCons[A](var head: A, var next: Stack[A]) extends Stack[A]
