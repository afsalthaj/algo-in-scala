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
}
