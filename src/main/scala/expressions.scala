package com.alvaro.expressions

object utils {
  import java.util.Calendar

  def time[A](message: String)(block: => A): A = {
    val start = System.nanoTime()
    println(s"Timing $message")
    val result = block
    val end = System.nanoTime()

    val differenceInMs = (end - start) / 1e6
    println(s"Duration = $differenceInMs ms")

    result
  }
}

sealed trait Expr {
  def print: String
  def eval: Option[Int]
  def simplify: Expr

  def prettyPrint: String = print
}

case class Number(value: Int) extends Expr {
  def eval = Some(value)
  def print = value.toString
  def simplify = this
}
case class Variable(name: String)(implicit
    variable_definitions: Map[String, Int]
) extends Expr {
  def eval = variable_definitions.get(name)
  def print = name
  def simplify = this
}
case class Sum(e1: Expr, e2: Expr) extends Expr {
  def eval = for {
    eval1 <- e1.eval
    eval2 <- e2.eval
  } yield (eval1 + eval2)
  def print = s"(${e1.print} + ${e2.print})"

  override def prettyPrint = (e1, e2) match {
    case (Variable(x), Variable(y)) => if (x < y) s"$x + $y" else s"$y + $x"
    case (s1: Sum, e: Expr) => s"${s1.prettyPrint} + ${e.prettyPrint}"
    case (e: Expr, s1: Sum) => s"${e.prettyPrint} + ${s1.prettyPrint}"
    case _ => s"(${e1.prettyPrint} + ${e2.prettyPrint})"
  }

  def simplify: Expr = (e1.simplify, e2.simplify) match {
    case (Number(0), e: Expr)     => e.simplify
    case (e: Expr, Number(0))     => e.simplify
    case (n: Number, m: Number)   => Number(n.value + m.value)
    case (n: Number, x: Variable) => Sum(x, n)
    case (x: Variable, Prod(n: Number, y: Variable)) if (x == y) =>
      Prod(Number(n.value + 1), x)
    case (Prod(n: Number, x: Variable), y: Variable) if (x == y) =>
      Prod(Number(n.value + 1), x)
    case (Sum(n: Number, x: Variable), Sum(m: Number, y: Variable)) =>
      Sum(Sum(x, y), Number(n.value + m.value))
    case (Prod(n: Number, x: Variable), Prod(m: Number, y: Variable))
        if (x == y) =>
      Prod(Number(n.value + m.value), x)
    case (e1: Expr, e2: Expr) if (e1 == e2) => Prod(Number(2), e1.simplify)
    case (e1: Expr, e2: Expr)               => Sum(e1.simplify, e2.simplify)
    case _                                  => Sum(e1, e2)
  }

}
case class Prod(e1: Expr, e2: Expr) extends Expr {
  def eval = for {
    eval1 <- e1.eval
    eval2 <- e2.eval
  } yield (eval1 * eval2)

  def print = s"(${e1.print} * ${e2.print})"

  override def prettyPrint = (e1, e2) match {
    case (Number(n), Variable(x)) => s"$n$x"
    case (Variable(x), Variable(y)) if (x == y) => s"$x ^ 2"
    case (Variable(x), Variable(y)) => if (x < y) s"$x$y" else s"$y$x"
    case (Number(n), prod: Prod) => s"$n${prod.prettyPrint}"
    case _ => s"(${e1.prettyPrint} * ${e2.prettyPrint})"
  }

  def simplify: Expr = (e1.simplify, e2.simplify) match {
    // Basic simplification
    case (Number(1), e: Expr)   => e.simplify
    case (e: Expr, Number(1))   => e.simplify
    case (n: Number, m: Number) => Number(n.value * m.value)

    // Distributive property
    case (Sum(e1, e2), e: Expr) =>
      Sum(
        Prod(e1, e).simplify,
        Prod(e2, e).simplify
      )
    case (e: Expr, Sum(e1, e2)) =>
      Sum(
        Prod(e1, e).simplify,
        Prod(e2, e).simplify
      )

    // Associativity and arrange numbers before variables
    case (Prod(n: Number, e: Expr), m: Number) =>
      Prod(Number(n.value * m.value), e.simplify)
    case (n: Number, Prod(m: Number, e: Expr)) =>
      Prod(Number(n.value * m.value), e.simplify)
    case (Prod(n: Number, x: Variable), Prod(m: Number, y: Variable)) =>
      Prod(Number(n.value * m.value), Prod(x, y))

    case (e: Expr, n: Number) => Prod(n, e.simplify)
    case (n: Number, e: Expr) => Prod(n, e.simplify)

    case _ => Prod(e1.simplify, e2.simplify)
  }
}

object test extends App {
  def report(expr: Expr) = {
    import utils.time

    val result: Expr = time("simplification") {
      expr.simplify
    }
    val initial_evaluation = expr.eval
    val simplified_evaluation = result.eval

    println(s"Variable definitions: $variable_definitions")
    assert(initial_evaluation == simplified_evaluation)
    println(s"Initial result:")
    println(s"${expr.print} = $initial_evaluation")
    println(s"Simplified result")
    println(s"${result.print} = ${simplified_evaluation}")
    println(s"Pretty printed result")
    println(s"${result.prettyPrint} = ${simplified_evaluation}")
    println()
  }

  implicit val variable_definitions = Map("x" -> 5, "y" -> 2)

  val expr = List(
    Sum(Prod(Number(5), Number(2)), Sum(Variable("x"), Variable("x"))),
    Prod(
      Sum(
        Sum(Number(2), Number(5)),
        Number(5)
      ),
      Prod(Variable("y"), Number(-1))
    ),
    Sum(
      Prod(Variable("x"), Number(2)),
      Prod(Variable("x"), Number(2))
    ),
    Sum(Number(1), Sum(Number(1), Sum(Number(1), Number(0)))),
    Sum(Variable("x"), Sum(Variable("x"), Sum(Variable("x"), Number(0)))),
    Prod(
      Sum(Prod(Number(3), Variable("x")), Number(5)),
      Prod(Prod(Number(4), Number(2)), Variable("x"))
    ),
    Prod(
      Sum(
        Sum(Number(5), Variable("x")),
        Prod(Prod(Variable("y"), Variable("x")), Number(4))
      ),
      Number(2)
    ),
    Prod(
      Sum(Number(2), Variable("x")),
      Sum(Number(3), Variable("x"))
    )
  )

  expr.foreach(report)

}
