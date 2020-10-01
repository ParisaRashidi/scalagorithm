package io.github.mahdyne





import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.abs

/**
 * @author mahdyne on 9/8/19.
 */
object HackerRank {
  case class Candid(i:Int,j:Int,value:Int)
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
  def hourglassSum(arr: Array[Array[Int]]): Int = {
    val candid=for{
      i<-(1 until  5)
      j<-(1 until 5)
    }yield (Candid(i,j,arr(i)(j)))
    val maxHourGlass=candid.map{c=>
      val hourGlass=for {
        i<- (c.i-1 to c.i+1)
        j<- (c.j-1 to c.j+1)
        if(i!=c.i)
      }yield arr(i)(j)
      hourGlass:+c.value sum
    }.max
    maxHourGlass
  }
  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    val bufArr=a.to[ArrayBuffer]
    (0 until d).foreach{i=>
      val head=bufArr(0)
      bufArr.remove(0)
      bufArr += head
    }
    bufArr.toArray
    /*@tailrec
    def circularRotate(arr:Array[Int],d:Int):Array[Int]=d match {
      case 0=>arr
      case _=>circularRotate(arr.drop(1):+arr(0),d-1)
    }
    circularRotate(a,d)*/
  }
  def rotLeftV2(a: Array[Int], d: Int): Array[Int] = {
    @tailrec
    def circularRotate(arr:Array[Int],d:Int):Array[Int]=d match {
      case 0=>arr
      case _=>circularRotate(arr.drop(1):+arr(0),d-1)
    }
    circularRotate(a,d)
  }
  def minimumBribes(q: Array[Int]) ={
    val bribedQueue=q.sorted.to[ArrayBuffer]
    var bribers:ArrayBuffer[(Int,Int)]=ArrayBuffer()
    var i=0
    while (i<q.length){
      if(q(i)-i>1)bribers += Tuple2(q(i),i)
      i+=1
    }
    val nbBribers=bribers.length
    val validBribers=bribers.filter(t=>t._1-t._2<=3)
    if(validBribers.length==nbBribers) {
      var nbMoves=0
      var i=0
      while(i<q.length){
        //println(bribedQueue)
        if(bribedQueue(i)!=q(i)){
          nbMoves+=bribedQueue.indexOf(q(i))-i
          bribedQueue.moveElem(bribedQueue.indexOf(q(i)),i)
          i=0
        }else{
          i+=1
        }
      }
      println(nbMoves.toString)
    }else
      println("Too chaotic")
  }
  def minimumBribesV2(q: Array[Int]) ={
    val zipped = q.zipWithIndex
    println(q.sorted.toList)
    println(q.toList)
    var i=0
    var res=""
    var nbMoves=0
    while(i<zipped.length){
      val elem=zipped(i)
      if(elem._1-elem._2>3) {
        res="Too chaotic"
        i=zipped.length
      } else if(elem._1-elem._2>1) {
        nbMoves+=elem._1-elem._2-1
        res=nbMoves.toString
        i+=1
      } else if(elem._1-elem._2==0  && zipped(elem._2-1)._1>elem._1 && elem._1>zipped(elem._2+1)._1) {
        nbMoves += 1
        res = nbMoves.toString
        i += 1
      }else if(elem._1-elem._2==1  && zipped(elem._2-1)._1>elem._1) {
        nbMoves += 1
        res = nbMoves.toString
        i += 1
      }else
        i+=1
      println(s"${zipped(i-1)._1},$res")
    }
    println(res)
  }

  def minimumSwaps2(arr: Array[Int]): Int = {
    val arrBuf = arr.toBuffer
    var i: Int = 0
    var swapCounter = 0
    while (i < arr.length) {
      var theNum = arrBuf(i)
      var locNum = theNum - 1
      if (i + 1 != theNum) {
        val swapIdx = arrBuf.zipWithIndex.drop(i+1).map(e => (e._2, abs(locNum - e._2) + abs(e._1 - 1 - i))).minBy(_._2)._1
        val tmp = arrBuf(swapIdx)
        arrBuf(swapIdx) = arrBuf(i)
        arrBuf(i) = tmp
        swapCounter += 1
        println(i,swapIdx,arrBuf)
      } else i += 1
    }
    swapCounter
  }
  def minimumSwaps3(arr: Array[Int]): Int = {
    var swapCounter=0
    var i=1
    var len=arr.length
    while(i<=len)
    {
      if(arr(i-1)!=i){
        val index=indexOfIntArray(arr,i,len)
        val tmp = arr(i-1)
        arr(i-1)=i
        arr(index)=tmp
        swapCounter+=1
      }
      i+=1
    }
    swapCounter
  }
  def sherlockAndAnagrams(s: String): Int = {
    s.inits
      .flatMap(_.tails)
      .withFilter(!_.isEmpty)
      .map(_.sorted)
      .toList
      .groupBy(identity)
      .map{case (_,v)=> ((v.size - 1) * v.size) / 2}
      .sum
  }

  def arrayManipulationV1(n: Int, queries: Array[Array[Int]]) = {
    val storage=(1 to n).toArray.map(t=>(0,t))
    queries.foreach(com=>storage.foreach{s=>storage(s._2-1)= if(s._2>=com(0) && s._2<=com(1)) (s._1+com(2),s._2) else (s._1,s._2)})
    println(storage.map(e=>e._1).toList)
  }
  def arrayManipulationV2(n:Int,queries:Array[Array[Int]])={
    queries
      .flatMap(e=>Range.inclusive(e(0),e(1)).map(r=>(r,e(2))))
      .groupBy(_._1).map{case (k,v)=>v.map(_._2.toLong).sum}.max
  }

  def checkMagazine(magazine: Array[String], note: Array[String]):String={
    val magazineMap=magazine.map(e=>(e,1)).groupBy(_._1).map(e=>e._1->e._2.map(_._2).sum)
    val noteMap=note.map(e=>(e,1)).groupBy(_._1).map(e=>(e._1,e._2.map(_._2).sum))
    if(noteMap.exists(e=>magazineMap.getOrElse(e._1,0)<e._2)) "NO" else "YES"
  }
  def twoStrings(s1: String, s2: String) = {
    val s1Set=s1.toSet
    val s2Set=s2.toSet
    val total=s1Set++s2Set
    if(total.size!=s1Set.size+s2Set.size) "YES" else "NO"
  }
  def countTriplets(arr: Array[Long], r: Long): Long = {
    arr.filter(e=>e%r==0 || e==1).toList.xcombinations(3).count(e=>e(0)*r==e(1) && e(1)*r==e(2))
  }
  def gameOfThrones(s: String): String = {
    val oddCount=s.toSeq.groupBy(identity).values.map(_.size).count(_ % 2 != 0)
    if(oddCount>1)
      "NO"
    else
      "YES"
  }
  def makingAnagrams(s1: String, s2: String): Int = {
    val intersect=s1.filter(e=>s2.contains(e))
    val intersectDelete=intersect.distinct.sorted.map{e=>
      val intersectCount=intersect.count(_==e)
      math.abs(s1.count(_==e)-intersectCount)+math.abs(s2.count(_==e)-intersectCount)
    }.sum
    intersectDelete+s1.filter(e=> !intersect.contains(e)).size + s2.filter(e=> !intersect.contains(e)).size
  }
  def anagram(s: String): Int = {
    val n=s.length
    if(n%2==0) {
      val (l1,l2)=s.toSeq.splitAt(n/2)
      n/2-l1.intersect(l2).length
    }else{
      -1
    }
  }

  def indexOfIntArray(array:Array[Int], key: Int, len:Int) = {
    var returnvalue = -1
    var i = 0
    while (i < len) if (key == array(i)) {
      returnvalue = i
      i=len
    }else i += 1
    returnvalue
  }
  def checkDuplicate(in: List[Int]) = {
    val acc = List()
    @tailrec
    def checkDuplicateAcc(in: List[Int], out: List[(Int, Int, String)]):List[(Int, Int, String)] = if (in.nonEmpty) if (in.size > 1 && in.head == in(1)) {
        val res = out :+ (in.head, in(1), "hit")
        checkDuplicateAcc(in.drop(2), res)
      } else checkDuplicateAcc(in.drop(1), out) else out
    checkDuplicateAcc(in, acc)
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
    def moveElem(originIdx:Int,destIdx:Int):List[A]= l.take(destIdx)++(l(originIdx)+:l.patch(originIdx,Nil,1).drop(destIdx))
  }
  implicit class CombinatorialBuffer[A](arr: ArrayBuffer[A]) {
    def moveElem(originIdx:Int,destIdx:Int):ArrayBuffer[A]= {
      val origin=arr(originIdx)
      var i = originIdx
      while (i > destIdx) {
        arr(i) = arr(i - 1)
        i -= 1
      }
      arr(destIdx) = origin
      arr
    }
  }
}