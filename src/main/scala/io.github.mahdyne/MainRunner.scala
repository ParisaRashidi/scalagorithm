package io.github.mahdyne
import Recursion._

import scala.annotation.tailrec
/**
 * @author mahdyne on 9/6/19.
 */
object MainRunner {
  def main(args: Array[String]): Unit = {
    val b=Array(10,5,20,20,4,5,2,25,1)
    HackerRank.breakingRecords(b).foreach(println)

  }
}
