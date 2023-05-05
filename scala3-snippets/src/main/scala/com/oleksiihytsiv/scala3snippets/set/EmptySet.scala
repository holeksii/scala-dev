package com.oleksiihytsiv.scala3snippets
package set

object EmptySet extends IntSet:
  def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet)
  def contains(x: Int): Boolean = false

  def union(s: IntSet): IntSet = s
