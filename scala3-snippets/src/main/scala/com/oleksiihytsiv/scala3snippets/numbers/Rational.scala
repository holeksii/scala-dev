package com.oleksiihytsiv.scala3snippets
package numbers

class Rational(x: Int, y: Int):
  require(y != 0, "denominator must be nonzero")

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  private val g: Int = gcd(x.abs, y.abs)

  def this(x: Int) = this(x, 1)

  def numer =
    if y < 0 then -x / g
    else x / g

  def denom =
    if y < 0 then -y / g
    else y / g

  def neg = Rational(-numer, denom)

  infix def add(r: Rational) =
    Rational(numer * r.denom + r.numer * denom, denom * r.denom)

  def mul(r: Rational) =
    Rational(numer * r.numer, denom * r.denom)

  def sub(r: Rational) = add(r.neg)

  def div(r: Rational) =
    Rational(numer * r.denom, denom * r.numer)

  def less(r: Rational) = numer * r.denom < r.numer * denom

  override def toString(): String = s"$numer/$denom"

extension (r: Rational)
  def min(r2: Rational) = if r.less(r2) then r else r2
  def abs = Rational(r.numer.abs, r.denom)

  def +(r2: Rational) = r.add(r2)
  def -(r2: Rational) = r.sub(r2)
  def *(r2: Rational) = r.mul(r2)
  def /(r2: Rational) = r.div(r2)
