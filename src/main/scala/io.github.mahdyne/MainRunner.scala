package io.github.mahdyne
import Recursion._

import scala.annotation.tailrec
/**
 * @author mahdyne on 9/6/19.
 */
object MainRunner {
  def main(args: Array[String]): Unit = {
    val b=Array(1,2,1,3,2)
    println(HackerRank.birthday(b,3,2))
  }
}
