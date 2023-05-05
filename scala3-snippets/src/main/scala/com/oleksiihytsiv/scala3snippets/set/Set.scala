package com.oleksiihytsiv.scala3snippets
package set

abstract class IntSet:
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
