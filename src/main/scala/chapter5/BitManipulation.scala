package chapter5

object BitManipulation {

  /**
   * 2's complement
   * The binary representation of-K (negative K) as a N-bit number is conecat ( 1, 2N -1 - K).
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

}
