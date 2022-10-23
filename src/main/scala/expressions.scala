package com.alvaro.expressions

import timing.utils.time

sealed trait Expr {
  def print: String
  def eval: Option[Int]
  def simplify: Expr

  def prettyPrint: String = print
  override def toString: String = print
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
    case (Variable(x), Variable(y)) => s"$x + $y"
    case (s1: Sum, e: Expr)         => s"${s1.prettyPrint} + ${e.prettyPrint}"
    case (e: Expr, s1: Sum)         => s"${e.prettyPrint} + ${s1.prettyPrint}"
    case _ => s"(${e1.prettyPrint} + ${e2.prettyPrint})"
  }

  def simplify: Expr = (e1.simplify, e2.simplify) match {
    // Basic simplification
    case (Number(0), e: Expr)     => e.simplify
    case (e: Expr, Number(0))     => e.simplify

    case (n: Number, m: Number)   => Number(n.value + m.value)
    case (n: Number, x: Variable) => Sum(x, n)

    // Ordered variables
    case (x: Variable, y: Variable) if (x.name > y.name) => Sum(y, x)
    case (x: Variable, Sum(y: Variable, n: Number))  => Sum(Sum(x, y).simplify, n)

    // Reductions
    case (n: Number, Sum(x: Variable, m: Number)) => Sum(x, n.value + m.value)
    case (Sum(x: Variable, n: Number), m: Number) => Sum(x, n.value + m.value)
    case (Sum(x: Variable, n: Number), Sum(y: Variable, m: Number)) =>
      Sum(Sum(x, y).simplify, Number(n.value + m.value))
    case (x: Variable, Prod(n: Number, y: Variable)) if (x == y) =>
      Prod(Number(n.value + 1), x)
    case (Prod(n: Number, x: Variable), y: Variable) if (x == y) =>
      Prod(Number(n.value + 1), x)
    case (Prod(n: Number, x: Variable), Prod(m: Number, y: Variable))
        if (x == y) =>
      Prod(Number(n.value + m.value), x)
    case (e1: Expr, e2: Expr) if (e1 == e2) => Prod(Number(2), e1.simplify)

    // general cases
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
    case (Number(n), Variable(x))               => s"$n$x"
    case (Variable(x), Variable(y)) if (x == y) => s"$x ^ 2"
    case (Variable(x), Variable(y)) => if (x < y) s"$x$y" else s"$y$x"
    case (Number(n), prod: Prod)    => s"$n${prod.prettyPrint}"
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

object Expr {
  implicit def intToNumber(n: Int): Number = Number(n)
  implicit def stringToVariable(name: String)(implicit
      variable_definitions: Map[String, Int]
  ): Variable = Variable(name)
}

object test extends App {
  def report(expr: Expr) = {
    println()
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
      Prod("x", 2),
      Prod("x", 2)
    ),
    Sum(1, Sum(1, Sum(1, 0))),
    Sum("x", Sum("x", Sum("x", 0))),
    Prod(
      Sum(Prod(3, "x"), 5),
      Prod(Prod(4, 2), "x")
    ),
    Prod(
      Sum(
        Sum(5, "x"),
        Prod(Prod("y", "x"), 4)
      ),
      2
    ),
    Prod(
      Sum(2, "x"),
      Sum(3, "x")
    )
  )

  expr.foreach(report)

}
