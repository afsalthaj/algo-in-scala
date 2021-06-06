package chapter6

object MathsLogicPuzzles {

  /**
   * is prime number
   *
   * remeember that n = a * b, and
   * there must be a factor a which is less than Math.sqrt(n),
   * for which mod(n, a) == 0, that makes it a non-prime.
   * This is because, if one is less than sqrt(n), the other should be
   * greater than sqrt(n)
   */
  def isPrime(n: Int) =
    if (n < 2) false
    else {
      val max: Double            = Math.sqrt(n)
      def go(next: Int): Boolean =
        if (next > max) false else if (n % next == 0) true else go(next + 1)

      go(2)
    }

  /**
   * Generate a list of prime numbers
   *
   * First generate a list of n numbers
   * then cancel out the ones divisible by 2.
   * then pick the next prime number in it.
   * cross of all the numbers divisible by that number.
   * and continue the processes until the next prime number is a none.
   * Finally you get a list of prime numbers
   *
   * Not so worried of space complexity here
   */
  def sieveOfEratosthenes(max: Int): List[Int] = {
    val allNumbers                = 2 to max
    val allNumbersNotDivisibleBy2 = allNumbers.filterNot(n => n % 2 == 0)

    def go(l: List[Int], prev: List[Int]): List[Int] =
      l.headOption match {
        case Some(value) =>
          val rest = l.tail.filter(n => n % value != 0)
          go(rest, value :: prev)
        case None        => l ++ prev
      }

    go(allNumbersNotDivisibleBy2.toList, Nil).reverse
  }
}
