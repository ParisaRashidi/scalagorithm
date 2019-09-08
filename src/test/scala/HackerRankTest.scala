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
}