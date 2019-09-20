package io.github.mahdyne

import scala.annotation.tailrec

/**
 * @author mahdyne on 9/8/19.
 */
object HackerRank {
  import CombinatorialOps._
  def gradingStudents(grades: Array[Int]): Array[Int] = {
    val multFive=Range.inclusive(1,21).map(_*5)
    grades.map { i =>
      if(i<38)
        i
      else {
        val sorted = (multFive :+ i).sorted
        val idx=sorted.indexOf(i)
        val rounded=sorted(idx+1)
        if(rounded-i<3) rounded else i
      }
    }
  }
  def countApplesAndOranges(s: Int, t: Int, a: Int, b: Int, apples: Array[Int], oranges: Array[Int]):Array[Int]={
    val applesLandOnHouse=apples.map(_+a).filter(p=> p<=t && p>=s)
    val orangesLandOnHouse=oranges.map(_+b).filter(p=> p<=t && p>=s)
    Array(applesLandOnHouse.length,orangesLandOnHouse.length)
  }
  def getTotalX(a: Array[Int], b: Array[Int]): Int = {
    val guesses=Range.inclusive(a.last,b.head)
    guesses.count(g => a.forall(g % _ == 0)  && b.forall(_ % g == 0))
  }
  def breakingRecords(scores: Array[Int]): Array[Int] = {
    @tailrec
    def findMaxCount(arr:List[Int],max:Int,counter:Int=0):Int= arr match{
      case Nil=>counter
      case x::xs=>if(x>max)findMaxCount(xs,x,counter+1) else findMaxCount(xs,max,counter)
    }
    @tailrec
    def findMinCount(arr:List[Int],min:Int,counter:Int=0):Int= arr match{
      case Nil=>counter
      case x::xs=>if(x<min)findMinCount(xs,x,counter+1) else findMinCount(xs,min,counter)
    }
    val scoreList=scores.toList
    val maxCount=findMaxCount(scoreList,scoreList.headOption.getOrElse(0))
    val minCount=findMinCount(scoreList,scoreList.headOption.getOrElse(0))
    Array(maxCount,minCount)
  }
  def birthday(s: Array[Int], d: Int, m: Int): Int = {
    s.sliding(m,1).toList.map(_.sum).count(_==d)
  }
  @tailrec
  def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    val newX1=x1+v1
    val newX2=x2+v2
    if(newX1==newX2)
      "YES"
    else if(newX1>newX2)
      "NO"
    else
      kangaroo(newX1,v1,newX2,v2)
  }
  def divisibleSumPairs(n: Int, k: Int, ar: Array[Int]): Int = {
    ar.toList.xcombinations(2) map(_.sum) count(_%k==0)
  }
}

object CombinatorialOps {
  implicit class CombinatorialList[A](l: List[A]) {
    def xcombinations(n: Int): List[List[A]] =
    if (n > l.size) Nil
    else l match {
      case _ :: _ if n == 1 =>
        l.map(List(_))
      case hd :: tl =>
        tl.xcombinations(n - 1).map(hd :: _) ::: tl.xcombinations(n)
      case _ => Nil
    }
  }
}