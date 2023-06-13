package com.oleksiihytsiv.scala3snippets
package list

import scala.annotation.tailrec
import java.util.NoSuchElementException

trait MyList[T]:
  def isEmpty: Boolean
  def head: T
  def tail: MyList[T]
  def apply(n: Int): T = nth(this, n)
  override def toString(): String =
    if isEmpty then "[]"
    else s"[ ${head.toString()}, ${tail.toString().substring(1, tail.toString().length() - 1)}]"

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

def MyList[T](xs: T*): MyList[T] =
  if xs.isEmpty then new MyNil[T]
  else new MyCons(xs.head, MyList(xs.tail*))

@main
def listMain: Unit =
  val list = MyList(1, 2, 3, 5, 6, 7, 8, 9)
  println(list)
