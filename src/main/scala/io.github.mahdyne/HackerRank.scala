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
  def migratoryBirds(arr: Array[Int]): Int = {
    val typeCount = arr.map((_,1)).groupBy(_._1).map{case (k:Int,v:Array[(Int,Int)])=>k->v.map(_._2).sum}
    typeCount.foreach(println)
    val max=typeCount.maxBy(_._2)._2
    val maxType=typeCount.filter(_._2==max).minBy(_._1)._1
    maxType
  }
  def dayOfProgrammer(year: Int): String = {
    val str=(day:Int,month:String,year:Int)=>s"$day.$month.$year"
    if(year>1918)
      if((year%4==0 && year%100!=0) || year%400==0) str(256-244,"09",year) else str(256-243,"09",year)
    else if(year<1918)
      if(year%4==0) str(256-244,"09",year) else str(256-243,"09",year)
    else str(256-243+13,"09",year)
  }
  def bonAppetit(bill: Array[Int], k: Int, b: Int): String = {
    val annaCharges=bill.zipWithIndex.filter(_._2!=k).map(_._1)
    val annaPortion=annaCharges.sum/2
    val overCharged=b-annaPortion
    if(overCharged>0) overCharged.toString else "Bon Appetit"
  }
  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    ar.groupBy(e=>e).map{case (k,v)=>v.length/2}.sum
  }
  def countingValleys(n: Int, s: String): Int = {
    var level=0
    var numValleys=0
    (0 until s.length).foreach{i=>
      if(s.charAt(i)=='U') {
        if(level== -1) numValleys += 1
        level+=1
      }
      if(s.charAt(i)=='D') {
        level+= -1
      }
    }
    numValleys
  }
  def jumpingOnClouds(c: Array[Int]): Int = {
    val cumulusIdxs=c.zipWithIndex.filter(_._1==0).map(_._2).toList
    @tailrec
    def cloud(ar:List[Int],steps:Int):Int= ar match{
        case Nil=>steps
        case x::xs=>xs.find(e=>e==x+2) match{
          case None=>cloud(ar.drop(1),steps+1)
          case Some(e)=>cloud(ar.dropWhile(_<x+2),steps+1)
        }
    }
    cloud(cumulusIdxs,0)-1
  }
  def repeatedString(s: String, n: Long): Long = {
    val aCountInS=s.count(_=='a')
    val countOfRepetitions=n/s.length
    val aIntheLastBucket=s.take((n-(countOfRepetitions*s.length)).toInt).count(_=='a')
    (countOfRepetitions*aCountInS)+aIntheLastBucket
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