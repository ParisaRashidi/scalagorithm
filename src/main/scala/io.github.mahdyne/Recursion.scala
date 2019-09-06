package io.github.mahdyne

import scala.annotation.tailrec

import scala.math.BigInt
/**
 * @author mahdyne on 9/6/19.
 */
object Recursion {
  def sum(list: List[Int]): Int = {
    @tailrec
    def sumHelper(list:List[Int],accumulator:Int):Int=list match{
      case Nil=>accumulator
      case x::xs=>sumHelper(xs,x+accumulator)
    }
    sumHelper(list,0)
  }
  def factorial(n:BigInt):BigInt= {
    val one=BigInt(1)
    @tailrec
    def factorialHelper(n:BigInt,accumulator:BigInt):BigInt=n match {
      case `one` => accumulator
      case x => factorialHelper(n-1,accumulator*n)
    }
    factorialHelper(n,1)
  }
  def fibonacci(x:Int):Int={
    def fibonacciHelper(x:Int,prev:Int=0,next:Int=1):Int=x match {
      case 0=>prev
      case 1=>next
      case _=>fibonacciHelper(x-1,next,next+prev)
    }
    fibonacciHelper(x)
  }
}
