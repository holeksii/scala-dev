package com.oleksiihytsiv.scala3snippets.set

class NonEmptySet(
    elem: Int,
    left: IntSet,
    right: IntSet,
  ) extends IntSet:
  def incl(x: Int): IntSet =
    if x < elem then new NonEmptySet(elem, left.incl(x), right)
    else if x > elem then new NonEmptySet(elem, left, right.incl(x))
    else this

  def contains(x: Int): Boolean =
    if x < elem then left.contains(x)
    else if x > elem then right.contains(x)
    else true

  def union(s: IntSet): IntSet =
    left.union(right).union(s).incl(elem)
