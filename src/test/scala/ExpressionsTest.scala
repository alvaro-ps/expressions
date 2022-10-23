import com.alvaro.expressions._

class SumExpressionsTest extends org.scalatest.funsuite.AnyFunSuite {
  implicit val variable_definitions = Map("x" -> 1, "y" -> 2)
  test("A sum of numbers is reducible") {
    val simpleSum = Sum(5, 5)
    assert(simpleSum.simplify === Number(10))
  }

  test("Number + Variable always in that order") {
    val simpleSum = Sum("x", 1)
    val simpleSumOutOfOrder = Sum(1, "x")

    assert(simpleSum.simplify === simpleSum)
    assert(simpleSumOutOfOrder.simplify === simpleSum)
  }

  test("The sum of N values result in one summed number") {
    val numbers = Sum(
      Sum(1, Sum(1, 1)),
      Sum(1, 1)
    )

    assert (numbers.simplify === Number(5))
  }
  
  test("Summing N variables returns N*x") {
    val variables = Sum("x", Sum("x", Sum("x", Sum("x", "x"))))

    assert(variables.simplify === Prod(5, "x"))
  }

  test("Some simple equation with 1 variable should simplify") {
    val expr = Sum(Sum(1, 2), Sum("x", 5))

    assert(expr.simplify === Sum("x", 8))
  }

  test("Some simple equation with 2 variables should simplify") {
    val expr = Sum(Sum(1, "y"), Sum("x", 5))

    assert(expr.simplify === Sum(Sum("x", "y"), 6))
  }

  test("A more complex equation with several variables should simplify") {
    val expr = Sum(Sum(Sum(1, "x"), Sum(2, "z")), Sum("y", 3))

    assert(expr.simplify === Sum(Sum("x", "x"), Sum("z", 6)))
  }
}
