package com.knoldus

class VowelCount {

  def getVowelsCount(str: String): Int = {
    val strLength = str.length
    val substringArray: Array[Int] = new Array[Int](strLength)
    for (counter <- 0 until strLength) {
      if (counter == 0) {
        substringArray(counter) = strLength
      } else {
        substringArray(counter) = (strLength - counter) + substringArray(counter - 1) - counter
      }
    }
    val vowelCounter: Array[Int] = new Array[Int](1)
    for (counter <- 0 until strLength) {
      if (isVowel(str.charAt(counter))) {
        vowelCounter(0) += substringArray(counter)
      }
    }
    vowelCounter(0)
  }

  def isVowel(char: Char): Boolean = {
    char match {
      case 'a' | 'A' => true
      case 'e' | 'E' => true
      case 'i' | 'I' => true
      case 'o' | 'O' => true
      case 'u' | 'U' => true
      case _ => false
    }
  }
}
