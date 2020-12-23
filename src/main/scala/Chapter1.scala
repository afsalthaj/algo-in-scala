object Chapter1 extends App {
  println(findPermutation("abc"))
  import scala.util.control.Breaks._

  /**
    * Is Unique: Implement an algorithm to determine if a string has all unique characters. What if you cannot use additional data structures?
    */
  def isUnique(s: String): Boolean = {
    val builder = new StringBuilder
    var isUnique = true

    breakable(for (c <- s) {
      if (builder.contains(c)) {
        isUnique = false
        break()
      } else {
        builder.append(c)
      }
    })

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

      for (s <- s1) {
        map.get(s).fold(map.update(s, 1))(count => map.update(s, count + 1))
      }

      map
    }

    val s11 = findCharacterCount(s1)
    val s22 = findCharacterCount(s2)

    (s1.length == s2.length) &&
    s11.forall({
      case (c, count) => s22.get(c).contains(count)
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
    var count = 0
    for (c <- myArray) {
      if (c.toString == " " &&
        (count > 0 && count < trueLength && myArray(count - 1) != "%20")) {
        myArray(count)= "%20"
      }

      count+=1
    }
    myArray.mkString
  }


  def findPermutation(s: String): Seq[String] = {
    var list: List[String] = Nil
    var count = -1

    def loop(prefix: String): String = {
      count += 1
      if (count == s.length - 1) {
        prefix
      } else {
        val remaining = s.substring(0, count) + s.substring(count + 1)
        list = list ++ List(prefix + remaining)
        loop(prefix + s.charAt(count))
      }

    }

    loop("")

    list
  }



  // abcd
  // a bc
  // a cb
  // ab c
  // ac
  // b ac
  // b ca
  // ba c
  // bc a
  //


}
