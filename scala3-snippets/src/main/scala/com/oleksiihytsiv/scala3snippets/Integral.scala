package com.oleksiihytsiv.scala3snippets

class MyIntegral(
    val a: Double,
    val b: Double,
    var n: Int,
    val f: Double => Double,
  ):
  lazy val h = (b - a) / n

  def trapezional: Double =
    val sum = (1 until n).map(i => f(a + i * h)).sum
    h * (f(a) + f(b) + 2 * sum) / 2
