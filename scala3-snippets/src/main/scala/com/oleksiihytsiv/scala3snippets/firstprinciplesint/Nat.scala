package com.oleksiihytsiv.scala3snippets.firstprinciplesint

abstract class Nat:
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
