package io.github.mahdyne
import Recursion._
/**
 * @author mahdyne on 9/6/19.
 */
object MainRunner {
  def main(args: Array[String]): Unit = {
    //val l=List(1,2,3,4)
    val l=Array(4,73,67,38,33)
    HackerRank.gradingStudents(l).foreach(println)
  }
}
