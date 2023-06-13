package com.oleksiihytsiv.scala3snippets.matcher

trait Expr:
  def show: String = this match
    case Number(n) => n.toString
    case Sum(e1, e2) => s"(${e1.show} + ${e2.show})"
    case Var(x) => x
    case Prod(e1, e2) => s"${e1.show} * ${e2.show}"

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(x: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

@main
def exprMain: Unit =
  println(Prod(Sum(Number(2), Number(4)), Prod(Var("x"), Number(2))).show)
