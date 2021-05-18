object Chapter2 {

  /**
   * A (mutable) singly linked list implementation,
   * with all the niceness of Scala.
   * The whole idea is revolving around pattern matching
   * and recursion to solve "almost" all algorithms.
   */
  sealed trait LinkedList[A] { self =>

    def current = self match {
      case Cons(head, tail) => Some(head)
      case Nil()            => None
    }

    def next: Option[A] = self match {
      case Cons(head, tail) => tail.headOption
      case Nil()            => None
    }

    def nextTail: LinkedList[A] = self match {
      case Cons(head, tail) =>
        tail match {
          case Cons(head, tail) => tail
          case Nil()            => Nil()
        }
      case Nil()            => Nil()
    }

    def fold[B](f: A => B)(g: (B, B) => B)(identity: B): B =
      (current, next) match {
        case (Some(v1), Some(v2)) =>
          val firstTwo = g(f(v1), f(v2))
          g(firstTwo, nextTail.fold(f)(g)(identity))
        case (Some(v1), None)     => g(f(v1), identity)
        case (None, Some(v2))     => g(f(v2), identity)
        case (None, None)         => identity
      }

    /**
     * 2.2: Return Kth to Last. Implement an algorithm
     * to find the Kth to last element of a singly linked list.
     */
    def returnKthToLast(k: Int) = {
      def go(rest: LinkedList[A], k0: Int): LinkedList[A] =
        self match {
          case Cons(head, tail) if k0 == k => tail
          case Cons(head, tail)            => go(tail, k0 + 1)
          case Nil()                       => Nil()
        }

      go(self, 1)
    }

    /**
     * 2.3 Delete Middle Node
     * Implement an algorithm to delete a node in the middle (i.e, any node but
     * the first and last node, not necessarily the exact middle) of a singly
     * linked list, given only access to the node.
     *
     * Essentially the solution, is jumping through all the nodes, and replace
     * the tail once it hits the middle node. Complexity is O(n).
     *
     * Note that, jumping through the node and replacing the tail results
     * in the original list (self) to be mutated
     */
    def deleteMiddleNode() =
      if (self.size > 2) {
        val middle                                          = self.size / 2
        def go(rest: LinkedList[A], k0: Int): LinkedList[A] =
          rest match {
            case a @ Cons(head, tail) if k0 == middle =>
              val removed = tail.moveLeft
              a.tail = removed
              self
            case a @ Cons(head, tail)                 => go(tail, k0 + 1)
            case Nil()                                => self
          }
        go(self, 1)
      } else self

    def headOption: Option[A] = self match {
      case Cons(head, tail) => Some(head)
      case Nil()            => None
    }

    /**
     * MoveLeft returns a new list, however zero copy.
     *
     * Cons(1, Cons(2, Nil())).moveLeft returns Cons(2, Nil())
     * which is simply a reference of tail to the above list
     * This is really useful when you want to delete an element from anywhere
     * in the list.
     *
     * Example: To delete the second element from a list
     *  (for instance, Cons(1, Cons(2, Nil())))
     *
     * list match {
     *   case Cons(h1, tail) =>
     *     val tailWithHeadRemoved = tail.moveLeft
     *     Cons(h1, tailWithHeadRemoved)
     *   case Nil() => Nil()
     * }
     */
    def moveLeft: LinkedList[A] =
      self match {
        case a @ Cons(_, tail) =>
          tail match {
            case Cons(h2, t2) =>
              a.head = h2
              a.tail = t2
              a
            case Nil()        => Nil()
          }
        case Nil()             => Nil()
      }

    /**
     * Prepend is a O(1) operation
     */
    def prepend(a: A): LinkedList[A] =
      Cons(a, self)

    /**
     * Append is a O(n) operation, with zero copy.
     * Using foldLeft and foldRight
     * is something that you can try though,
     * but make sure you are not building a new structure (list)
     * in memory (i.e, zero copy)
     */
    def append(a: A): LinkedList[A] = {
      def mutate(acc: LinkedList[A]): Unit =
        acc match {
          case a2 @ Cons(h, Nil()) => a2.tail = Cons(a, Nil())
          case a2 @ Cons(h, t)     => mutate(t)
          case Nil()               => ()
        }
      mutate(self)
      self
    }

    /**
     * A O(n) reverse with copy involved
     * We are accumulating, or generating a new list
     */
    def reverseInEfficient: LinkedList[A] = {
      def go(rem: LinkedList[A], acc: LinkedList[A]): LinkedList[A] =
        rem match {
          case Cons(head, tail) => go(tail, Cons(head, acc))
          case Nil()            => acc
        }

      go(self, Nil())
    }

    def foreach(f: A => Unit): Unit =
      self match {
        case Cons(head, tail) =>
          f(head)
          tail.foreach(f)
        case Nil()            => ()
      }

    /**
     *  This is sort of a suboptimal "Zipper" where a focus
     *  element knows the previous and next; Something that
     *  can be useful for moving average. In fact a Zipper
     *  case class Zipper(left: Stream[A], focus: A, next: Stream[A])
     *  is super powerful in many algorithms. We will discover this
     *  as we go forward.
     *
     * Example:
     *  println(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil()))))))
     *   .sliderForeach((a, b, c) => println(a + b + c)))
     *  6
     *  9
     *  12
     *  15
     *  ()
     */
    def sliderForeach(f: (A, A, A) => Unit): Unit = {
      def go(prev: Option[A], rest: LinkedList[A]): Unit =
        rest match {
          case Cons(current, tail) =>
            tail.headOption match {
              case Some(next) =>
                prev match {
                  case Some(prev) =>
                    f(prev, current, next)
                    go(Some(current), tail)
                  case None       =>
                    go(Some(current), tail)
                }

              case None => ()
            }
          case Nil()               => ()
        }

      go(None, self)
    }

    def size: Int = {
      def go(acc: Int, remaining: LinkedList[A]): Int =
        remaining match {
          case Cons(_, tail) => go(acc + 1, tail)
          case Nil()         => acc
        }

      go(0, self)
    }
  }

  case class Cons[A](var head: A, var tail: LinkedList[A]) extends LinkedList[A]
  case class Nil[A]()                                      extends LinkedList[A]

  /**
   * Text book question
   * 2.1 Write code to remove duplicates from an unsorted linked list
   */
  def removeDupicates(list: LinkedList[Int]): LinkedList[Int] =
    list match {
      case a @ Cons(h1, originalTail) => // 1, (1, 2, 1)
        def moveAllToLeft(updated: LinkedList[Int]): LinkedList[Int] =
          updated match { // Expected (1, 2)
            case a @ Cons(h2, _) if h1 == h2 =>
              val newList = a.moveLeft
              moveAllToLeft(newList)

            case a @ Cons(h2, nextTail) =>
              val checkForTail = moveAllToLeft(nextTail)
              Cons(h2, checkForTail)

            case Nil() => updated
          }

        val updatedTail = moveAllToLeft(originalTail)
        val newTail     = removeDupicates(updatedTail)
        Cons(h1, newTail)
      case Nil()                      => list
    }

  /**
   * 2.4 Write code to partition a linked list around a value x,
   * such that all nodes less than x come before all nodes greater
   * than or equal to x. If x is contained withint the list, the values
   * of x only need to be after the elements less than x. The partition
   * element x can can appear anywhere in the "right partition",
   * and it doesn't need to appear between the left and right partition.
   */
  def partition(list: LinkedList[Int], number: Int) = {
    def go(left: LinkedList[Int], right: LinkedList[Int]): LinkedList[Int] =
      left match {
        case a @ Cons(head, tail) =>
          if (tail == Nil()) {
            a.tail = right
            list
          } else {
            if (a.head >= number) {
              val newList = a.moveLeft
              go(newList, right.prepend(head))
            } else {
              go(tail, right)
            }
          }
        case Nil()                =>
          list
      }

    go(list, Nil())
  }

  /**
   * You have 2 numbers represented by a linked list, where each node
   * contains a single digit. The digits are stored in reverse order, such
   * that the 1's digit is at the head of the list. Write a function that adds the
   * two numbers and returns the sum as a linked list.
   */
  def sumTwoNumbers(left: LinkedList[Int], right: LinkedList[Int]): LinkedList[Int] = {
    println(left.reverseInEfficient)
    println(right.reverseInEfficient)
    val chars = ((left.reverseInEfficient.fold(_.toString)((a, b) => a + b)("")).toInt +
      (right.reverseInEfficient.fold(_.toString)((a, b) => a + b)("")).toInt).toString().toArray

    println(chars.toList.map { r =>
      println("the input is " + r + " and the output is " + r.toInt)
    })

    var output: LinkedList[Int] = Nil()

    for (char <- chars)
      output = output.prepend(char - '0') // https://stackoverflow.com/questions/16241923/scala-char-to-int-conversion

    output
  }
}

object RunChapter2 extends App {
  import Chapter2._

  println(partition(Cons(3, Cons(5, Cons(8, Cons(5, Cons(10, Cons(2, Cons(1, Nil()))))))), 5))
  println(partition(Cons(5, Cons(4, Cons(6, Cons(1, Cons(2, Cons(3, Nil())))))), 3))
  println(sumTwoNumbers(Cons(7, Cons(1, Cons(6, Nil()))), Cons(5, Cons(9, Cons(2, Nil())))))
  println(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))))))).deleteMiddleNode())
  println(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil())))))).sliderForeach((a, b, c) => println(a + b + c)))
  println(removeDupicates(Cons(1, Cons(2, Cons(1, Cons(2, Cons(1, Cons(3, Cons(4, Cons(3, Nil()))))))))))
}
