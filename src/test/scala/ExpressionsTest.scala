import com.alvaro.expressions._

class ExpressionsTest extends org.scalatest.funsuite.AnyFunSuite {
  test("A sum of numbers is reducible") {
    assert(Sum(Number(5), Number(5)) === Some(10))
  }
}
