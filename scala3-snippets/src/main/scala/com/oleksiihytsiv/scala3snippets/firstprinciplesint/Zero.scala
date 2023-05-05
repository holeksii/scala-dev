package com.oleksiihytsiv.scala3snippets.firstprinciplesint

object Zero extends Nat:
  override def isZero: Boolean = true
  override def successor: Nat = Succ(this)
  override def predecessor: Nat = ???
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = if that.isZero then this else ???
  override def toString(): String = "Zero"
