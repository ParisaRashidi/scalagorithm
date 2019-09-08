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
}
