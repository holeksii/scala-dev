package com.oleksiihytsiv.scala3snippets
package list

import scala.annotation.tailrec
import java.util.NoSuchElementException

trait MyList[T]:
  def isEmpty: Boolean
  def head: T
  def tail: MyList[T]

class MyNil[T] extends MyList[T]:
  def isEmpty: Boolean = true
  def head: Nothing = throw NoSuchElementException("Nil.head")
  def tail: Nothing = throw NoSuchElementException("Nil.tail")

class MyCons[T](val head: T, val tail: MyList[T]) extends MyList[T]:
  def isEmpty: Boolean = false

@tailrec
def nth[T](xs: MyList[T], n: Int): T =
  if xs.isEmpty then throw IndexOutOfBoundsException()
  else if n == 0 then xs.head
  else nth(xs.tail, n - 1)
