import io.github.mahdyne.Recursion
import org.scalatest.{FlatSpec, FunSuite, Matchers}

/**
 * @author mahdyne on 9/7/19.
 */
class RecursionTest extends FunSuite with Matchers{
  test("FibonacciTailRec"){
    assert(Recursion.fibonacci(8)===21)
  }
  test("FactorialTailRec"){
    noException should be thrownBy Recursion.fibonacci(10000)
  }
}