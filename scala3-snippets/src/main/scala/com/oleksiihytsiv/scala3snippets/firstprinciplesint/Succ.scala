package com.oleksiihytsiv.scala3snippets.firstprinciplesint

class Succ(n: Nat) extends Nat:
  override def isZero: Boolean = false
  override def successor: Nat = Succ(this)
  override def predecessor: Nat = n
  override def +(that: Nat): Nat = Succ(n + that)
  override def -(that: Nat): Nat =
    if that.isZero then this else n - that.predecessor
  override def toString(): String = s"Succ($n)"
