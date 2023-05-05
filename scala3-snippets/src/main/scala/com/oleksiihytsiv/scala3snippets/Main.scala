package com.oleksiihytsiv
package scala3snippets

import com.oleksiihytsiv.scala3snippets.numbers.{ Rational, abs }

@main
def Main(args: String*): Unit =
  println("─" * 50)

  val r1 = Rational(2, -5)
  val r2 = Rational(2, 3)
  println(r1)
  println(r1 add r2)
  println(r1.abs)

  println("─" * 50)

// @main
def birthday(name: String, age: Int) =
  print(s"Happy birthday, $name! You are $age years old already")
