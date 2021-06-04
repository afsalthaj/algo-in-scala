package chapter5

object BitManipulation {

  /**
   * 2's complement
   * The binary representation of-K (negative K) as a N-bit number is c one at ( 1, 2N -1 - K).
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
  def shifts() = ???
}
