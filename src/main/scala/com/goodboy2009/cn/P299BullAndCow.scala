package com.goodboy2009.cn

import scala.collection.mutable

object P299BullAndCow {

  /**
    * see https://leetcode.com/problems/bulls-and-cows
    *
    * 输入: secret = "1807", guess = "7810"
    * 输出: "1A3B"
    * 解释: 1 公牛和 3 奶牛。公牛是 8，奶牛是 0, 1 和 7。
    *
    * @param secret
    * @param guess
    * @return
    */

  def getHint(secret: String, guess: String): String = {
    val secretArr = secret.toList
    val guessArr = guess.toList
    var bulls = 0
    var cowSet = new mutable.HashSet[Char]()
    for (i <- 0 to secretArr.length - 1) {
      if (guessArr(i).equals(secretArr(i))) {
        bulls = bulls + 1
      } else {
        if (secretArr.contains(guessArr(i))) {
          cowSet.+=(guessArr(i))
        }
      }
    }
    return "%dA%dB".format(bulls, cowSet.size)
  }


  def getHint_Pass(secret: String, guess: String): String = {
    var bulls, cows = 0
    val arr = Array.ofDim[Int](10)

    (digits(secret) zip digits(guess)).foreach {
      case (s, g) if s == g => bulls += 1
      case (s, g) =>
        if (arr(s) < 0) cows += 1
        if (arr(g) > 0) cows += 1
        arr(s) += 1
        arr(g) -= 1
    }


    (digits(secret) zip digits(guess)).foreach(println(_))

    return  s"${bulls}A${cows}B";
  }


  def digits(number: String): IndexedSeq[Int] = number.map(_ - '0')

  def main(args: Array[String]): Unit = {
    val result = getHint_Pass("1807", "7810")
    assert(result.equals("1A3B"))
    assert(getHint_Pass("1123", "0111").equals("1A1B"))
    println(getHint_Pass("1", "1"))
    println(getHint_Pass("1122", "2211"))
  }
}
