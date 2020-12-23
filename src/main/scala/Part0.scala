import com.monovore.decline.{CommandApp, Opts}

import scala.annotation.tailrec
import cats.implicits._
import com.monovore.decline._

/**
  * Insertion sort
  */
object Part0 extends App {
  import Algorithms._

  // Cracking the coding interview - Read BigO chapter now and then.
  println(insertSort(Array(45, 2, 3)).toList)
}

object Algorithms {

  /**
    * In fact, elements A[1 .. j-1􏰏] are the elements originally in positions 1 through j-1, but now in sorted order.
    * This is called loop invariant. Probably because that subsection array is self contained and the values don't come from anywhere else.
    *
    * We use loop invariants to help us understand why an algorithm is correct. We must show three things about a loop invariant:
    *
    * It is true prior to the first iteration of the loop.
    * Maintenance: If it is true before an iteration of the loop, it remains true before the
    * Termination: When the loop terminates, the invariant gives us a useful property that helps show that the algorithm is correct.
    * next iteration.
    *
    * It is similar to mathematical induction.
    *
    * To begin with in the loop starts with i=0 and j=1
    * The array array(0, 0) which is the first element is true and is already satisfying your pbm.
    *
    * So the intialisation is true.
    * Now, in the next iteration, if the previous iteration is true, just maintain this trueness.
    *
    * Intro to algorithm book: Page 20, read termination
    */
  def insertSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def sort(j: Int): Array[Int] = {
      if (j >= array.length) {
        array
      } else {
        var i = j - 1
        while (i >= 0) {
          if (array(i) > array(j)) {
            swap(array, i, j)
          }
          i -= 1;
        }

        sort(j + 1)
      }
    }

    sort(1)
  }

  def swap(array: Array[Int], i: Int, j: Int): Array[Int] = {
    val tmp = array(i)
    array(i) = array(j)
    array(j) = tmp

    array
  }
}
