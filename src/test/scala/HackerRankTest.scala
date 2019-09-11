import io.github.mahdyne.HackerRank
import org.scalatest.{FunSuite, Matchers}

/**
 * @author mahdyne on 9/9/19.
 */
class HackerRankTest extends FunSuite with Matchers{
  test("gradingStudents"){
    val a=Array(4,73,67,38,33)
    assert(HackerRank.gradingStudents(a)===Array(4,75,67,40,33))
  }
  test("countApplesAndOranges"){
    val s=7
    val t=11
    val a=5
    val b=15
    val applesD=Array(-2,2,1)
    val orangesD=Array(5,-6)
    assert(HackerRank.countApplesAndOranges(s,t,a,b,applesD,orangesD)===Array(1,1))
  }
}