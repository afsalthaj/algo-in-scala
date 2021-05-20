package chapter1

import scala.annotation.tailrec

object Intro {
  import scala.util.control.Breaks._

  /**
   * Is Unique: Implement an algorithm to determine if a string has all unique characters. What if you cannot use additional data structures?
   */
  def isUnique(s: String): Boolean = {
    val builder  = new StringBuilder
    var isUnique = true

    breakable(
      for (c <- s)
        if (builder.contains(c)) {
          isUnique = false
          break()
        } else {
          builder.append(c)
        }
    )

    isUnique
  }

  /*
   * Check Permutation: Given two strings,write a method to decide if one is a permutation of the
   */
  def isPermutation(s1: String, s2: String) = {
    // if length of both strings are same,
    // and if the occurance of each character is same as in the other

    def findCharacterCount(s1: String) = {
      val map = scala.collection.mutable.Map[Char, Int]()

      for (s <- s1)
        map.get(s).fold(map.update(s, 1))(count => map.update(s, count + 1))

      map
    }

    val s11 = findCharacterCount(s1)
    val s22 = findCharacterCount(s2)

    (s1.length == s2.length) &&
    s11.forall({ case (c, count) =>
      s22.get(c).contains(count)
    })

  }

  /**
   * URLify: Write a method to replace all spaces in a string with '%20'.
   * You may assume that the string has sufficient space at the end to hold
   * the additional characters,and that you are given the "true" length of the string.
   * (Note: If implementing in Java,please use a character array so that
   * you can perform this operation in place.)
   */
  def urlify(s: String, trueLength: Int): String = {
    val myArray = s.toArray.map(_.toString)
    var count   = 0
    for (c <- myArray) {
      if (
        c.toString == " " &&
        (count > 0 && count < trueLength && myArray(count - 1) != "%20")
      ) {
        myArray(count) = "%20"
      }

      count += 1
    }
    myArray.mkString
  }

  /**
   * Find the permutation of string
   * println(findPermutation("abc"))
   * // List(abc, acb, bac, bca, cab, cba)
   */
  def findPermutations(originalString: String): List[String] = {
    def findPermutation(loopString: String, prefix: String, list: List[String]): List[String] =
      if (prefix.length == originalString.length)
        prefix :: list
      else {
        for {
          index <- (0 until loopString.length()).toList
          rem    = loopString.substring(0, index) + loopString.substring(index + 1)
          pre    = prefix + loopString.charAt(index)
          res   <- findPermutation(rem, pre, list)
        } yield res
      }

    findPermutation(originalString, "", Nil)
  }

  /**
   * Palindrome Permutation: Given a string, write a function to check if it is a permutation of a palin­ drome. A palindrome is a word or phrase
   * that is the same forwards and backwards. A permutation is a rearrangement of letters. The palindrome does not need to be limited to just
   * dictionary words.
   * EXAMPLE
   * Input: Tact Coa
   * Output: True (permutations: "taco cat", "atco eta", etc.)
   */
  def palindromePermutation(string: String) = {
    val listOfPermutations   = findPermutations(string)
    var result: List[String] = Nil

    for (eachPermutation <- listOfPermutations) {
      val lowered              = eachPermutation.toLowerCase
      var reversed: List[Char] = List()
      for (eachChar <- lowered)
        if (eachChar.toString != " ") {
          reversed = eachChar :: reversed
        }

      if (reversed.mkString == lowered.filterNot(_.toString == " ")) {
        result = eachPermutation :: result
      }
    }

    result
  }

  /**
   * One Away: There are three types of edits that can be performed on strings: insert a character, remove a character, or replace a character.
   * Given two strings, write a function to check if they are one edit (or zero edits) away.
   * EXAMPLE
   * pale, ple -> true pales, pale -> true pale, bale -> true pale, bake -> false
   */
  def oneWay(s1: String, s2: String): Int = {
    @tailrec
    def loop(s1: Array[Char], s2: Array[Char], edits: Int): Int =
      (s1.headOption, s2.headOption) match {
        case (Some(a), Some(b)) if a == b =>
          loop(s1.tail, s2.tail, edits)

        case (Some(a), Some(b)) =>
          if (s1.length > s2.length)
            loop(s1.tail, s2, edits + 1)
          else if (s2.length > s1.length)
            loop(s1, s2.tail, edits + 1)
          else
            loop(s1.tail, s2.tail, edits + 1)

        case (Some(_), None) =>
          loop(s1.tail, s2, edits + 1)

        case (None, Some(_)) =>
          loop(s1, s2.tail, edits + 1)

        case (None, None) => edits
      }

    loop(s1.toCharArray, s2.toCharArray, 0)
  }

  /**
   * String Compression: Implement a method to perform basic string compression
   * using the counts of repeated characters. For example, the string
   * aabcccccaaa would become a2blc5a3. If the "compressed" string would
   * not become smaller than the original string, your method should return
   * the original string. You can assume the string has only uppercase and lowercase letters (a - z).
   */
  def compressString(string: String): String = {
    @scala.annotation.tailrec
    def loop(string: Array[Char], previousChar: Option[Char], count: Int, stringBuilder: StringBuilder): StringBuilder =
      string.headOption match {
        case a @ Some(value) if a == previousChar =>
          loop(string.tail, Some(value), count + 1, stringBuilder)
        case Some(value) if previousChar.isEmpty  =>
          loop(string.tail, Some(value), 1, stringBuilder)
        case a @ Some(value) if a != previousChar =>
          loop(
            string.tail,
            Some(value),
            1,
            previousChar
              .map(c => stringBuilder.append(c.toString + count))
              .getOrElse(stringBuilder)
          )
        case None                                 =>
          previousChar
            .map(c => stringBuilder.append(c.toString + count))
            .getOrElse(stringBuilder)
      }

    loop(string.toCharArray, None, 0, new StringBuilder).toString
  }

  /**
   * Rotate Matrix: Given an image represented by an NxN matrix, where each pixel in
   * the image is 4 bytes, write a method to rotate the image by 90
   * degrees. Can you do this in place?
   * Hints:#51, #100
   *
   * Input:
   *
   * 1 2 3 4
   * 5 6 7 8
   * A B C D
   * E F G H
   *
   * Output:
   *
   * E A 5 1
   * F B 6 2
   * G C 7 3
   * H D 8 4
   */
  def rotateImage(input: Array[Array[Any]]): Array[Array[Any]] = {
    val size       = input.length
    val outerIndex = size - 1

    for (layer <- 0 until size) {
      @scala.annotation.tailrec
      def loop(i: Int, j: Int): Array[Array[Any]] =
        if (i < (outerIndex - layer) && j >= 0) {
          input
        } else {
          val temp = input(layer)(i)
          input(layer)(i) = input(j)(layer)
          input(j)(layer) = input(outerIndex - layer)(j)
          input(outerIndex - layer)(j) = input(i)(outerIndex - layer)

          input(i)(outerIndex - layer) = temp

          loop(i + 1, j + 1)
        }

      loop(layer, outerIndex - layer)
    }

    input
  }
}
