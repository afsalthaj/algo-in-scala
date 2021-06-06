package chapter5

object BitManipulation {

  /**
   * 2's complement
   * The binary representation of-K (negative K) as a N-bit number is concat ( 1, Math.pow(2, N -1) - K).
   *
   * 3: 011
   * BitManipulation.twosComplement(3, 4)
   * 1101 (representing -3, where 1st 1 indicates sign negative).
   *
   * Another way to look at this is that we invert the bits in the positive representation and then add 1. 3 is 011
   * in binary. Flip the bits to get 100, add 1 to get 101, then prepend the sign bit (1) to get 1101.
   *
   * 0 000: 0
   * flipped
   * 1 111 : -1 (reason why a sequence of 1 is -1)
   *
   * In other words,
   */
  def twosComplement(n: Int, numberOfBits: Int): String =
    "1" + (Math.pow(2, numberOfBits - 1) - n).toInt.toBinaryString

  /**
   * A quick overview of logic and arithmetic right shift:
   * -------------------------------------------------------
   * In a logical right shift, we shift the bits and put a 0 in the most significant bit.
   * It is indicated with a >>> operator.
   *
   * Example, in scala
   *
   * @ 4 >> 1 [ implies, 100 >> 1, ==> 010 ]
   * res3: Int = 2
   *
   * 10110101  : -75
   * 01011010  : 90
   *
   * Arithmetic shifts on the other hand divides the number by 2.
   * In an arithmetic right shift,
   * we shift values to the right but fill in the new bits with the value of the sign bit.
   *
   * Thus:
   * 10110101  : -75
   * 11011010  : -38
   */
  /**
   * Result wil be either 0 or 1
   */
  def getBit(n: Int, index: Int): Int = {
    val x = (1 << index) & n
    if (x == 0) 0 else 1
  }

  /**
   * Result will be any number that
   * is an outcome of setting the ith bit
   * of n
   */
  def setBit(n: Int, i: Int): Int =
    (1 << i) | n

  /**
   * This method operates in almost the reverse of setBit.
   * First, we create a number like 11101111 by creating
   * the reverse of it (00010000) and negating it.
   * Then, we perform an AND with num.
   * This will clear the ith bit and leave the remainder unchanged.
   */
  def clearBit(n: Int, i: Int): Int =
    n & ~(1 << i)

  /**
   * To clear all bits from the most significant bit through i (inclusive),
   * we create a mask with a 1 at the ith bit (1 << i).
   * Then,we subtract 1 from it, giving us a sequence of 0s followed by i 1s.
   * We then AND our number with this mask to leave just the last i bits.
   */
  def clearBitsMSBthroughI(n: Int, i: Int): Int =
    n & ((1 << i) - 1)

  /**
   * To clear all bits from i through 0 (inclusive),
   * we take a sequence of all ls (which is -1)
   * and shift it left by i + 1bits.
   * This gives us a sequence of 1s
   * (in the most significant bits) followed by i 0 bits.
   */
  def clearBitsMSBthrough0(n: Int, i: Int): Int =
    n & (-1 << (i + 1))

  /**
   * Update Bit
   * To set the ith bit to a value v,
   * we first clear the bit at position
   * i by using a mask that looks like 11101111.
   * Then, we shift the intended value, v , left by i bits.
   * This will create a number with bit i equal
   * to v and all other bits equal to 0.
   * Finally, we OR these two numbers,
   * updating the ith bit if v is 1 and leaving it as 0 otherwise.
   */
  def updateBit(n: Int, i: Int, bitIs1: Boolean) = {
    val value   = if (bitIs1) 1 else 0
    val mask    = ~(1 << i) //11110111
    val cleared = (n & mask)
    (cleared | (value << i))
  }

  /**
   * Insertion: You are given two 32-bit numbers, N and M, and two bit positions, i and j.
   * Write a method to insert M into N such that M starts at bit j and ends at bit i. You can assume that the bits j through i have enough space to fit all of M. That is, if M = 10011, you can assume that there are at least 5 bits between j and i. You would not, for example, have j = 3 and i = 2, because M could not fully fit between bit 3 and bit 2.
   * EXAMPLE
   * Input: N 10000000000, M 10011, i 2, j 6 Output:N = 10001001100
   *
   * @ Integer.parseInt("10000000000",2)
   * res2: Int = 1024
   *
   * @ Integer.parseInt("10011",2)
   * res3: Int = 19
   *
   * // Expected
   * @ Integer.parseInt("10001001100",2)
   * res4: Int = 1100
   *
   * // Real
   * @ insertion(1024, 19, 2, 6)
   * res7: Int = 1100
   */
  def insertion(n: Int, m: Int, i: Int, j: Int) = {
    val mask    = (-1 << i) + (1 << (j + 1))
    val negated = ~mask
    (negated & n) | m << i
  }

  /**
   * Binary to String: Given a real number between O and 1 (e.g., 0.72)
   * that is passed in as a double, print the binary representation.
   * If the number cannot be represented accurately in binary with at most 32 characters, print "ERROR:'
   *
   * Hint from text book:
   *
   * .56 = 5 * 1/10^1 + 6 * 1/ 10^2
   *
   * .101 = 1 * 1/2^1 + 0 * 1/2^2 + 1 * 1/2^3
   *
   * This implies
   * if 2 * .101(base 2) > 1(base 10), then the first element (in binary) is a 1.
   *
   * If so, append the builder by 1.
   *
   * Mostly, this is a formal technique that you just need to grasp.
   */
  def binaryToString(n: Double) = {
    val builder = new StringBuilder
    builder.append('.')

    def go(n: Double): String =
      if (n > 0) {
        val doubled = n * 2 // implies the first number is binary 1
        if (doubled > 1) {
          builder.append(1)
          go(doubled - 1)
        } else {
          builder.append(0)
          go(doubled)
        }
      } else {
        builder.toString()
      }

    go(n)
  }

  /**
   * Flip Bit to Win: You have an integer and you can flip exactly one bit from a O to a 1. Write code to find the length of the longest sequence of 1 s you could create.
   * EXAMPLE
   * Input: 1775 (or: 11011101111) Output: 8
   *
   * 4 -> 4
   *
   * 5
   * 6
   * 7 111
   *
   * 8 - 3 = 5
   *
   * 11011101111
   */

  type IndexToSet            = Int
  type Total1sBeforeAndAfter = Int
  type ShouldCountOrNot      = Boolean

  def flipBit(binaryString: String) = ???
}
