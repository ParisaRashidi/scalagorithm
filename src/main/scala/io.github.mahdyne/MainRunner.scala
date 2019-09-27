package io.github.mahdyne
import Recursion._

import scala.annotation.tailrec
/**
 * @author mahdyne on 9/6/19.
 */
object MainRunner {

  def main(args: Array[String]): Unit = {
    val s = "epsxyyflvrrrxzvnoenvpegvuonodjoxfwdmcvwctmekpsnamchznsoxaklzjgrqruyzavshfbmuhdwwmpbkwcuomqhiyvuztwvq"
    val n:Long=549382313570l //expected:16481469408
    val aCountInS=s.count(_=='a')
    val countOfRepetitions=n/s.length
    val aIntheLastBucket=s.take((n-(countOfRepetitions*s.length)).toInt).count(_=='a')
    val aCount=HackerRank.repeatedString(s,n)
   /* println(s.length,aCountInS,n)
    println(aCountInS)
    val aCount=
      if(aCountInS==0)
        0
      else if(s.startsWith("a"))
        math.ceil((n*aCountInS)/s.length.toDouble).toLong
      else
        ((n*aCountInS)/s.length.toDouble).toLong*/
    println(aCount)
  }

  def repeatedString(s: String, n: Long): Long = {
    var repeatCount = math.ceil(n / s.length.toDouble).toLong
    var concatStr = ""
    while (repeatCount > 0) {
      concatStr = concatStr.concat(s)
      repeatCount -= 1
    }
    var aCount: Long = 0
    println(concatStr)
    var nn = n
    while (nn > 0) {
      if (concatStr(0) == 'a') {
        aCount += 1
        println(concatStr, aCount)
        concatStr = concatStr.drop(1)
      } else concatStr = concatStr.drop(1)
      nn -= 1
    }
    aCount
  }
}