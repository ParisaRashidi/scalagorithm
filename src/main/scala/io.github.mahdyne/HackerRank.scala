package io.github.mahdyne

/**
 * @author mahdyne on 9/8/19.
 */
object HackerRank {
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
}
