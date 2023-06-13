package com.oleksiihytsiv
package scala3snippets

import scala.util.Random

@main
def Main(args: String*): Unit =
  val _1 = List(1, 1, 1)
  val _2 = List(2, 2, 2)
  val _3 = List(3, 3, 3)

  println(_1 ++ _2 ++ _3)

def mapFun[T, U](xs: List[T], f: T => U): List[U] = xs.foldRight(List[U]())((y, ys) => f(y) :: ys)

def lenFun[T](xs: List[T]): Int = xs.myFoldRight(0)((x, n) => n + 1)

extension [T](xs: List[T])
  def myFoldRight[U](acc: U)(op: (T, U) => U): U = xs match
    case Nil => acc
    case y :: ys => op(y, ys.foldRight(acc)(op))

  def myMap[U](f: T => U): List[U] = xs match
    case Nil => Nil
    case y :: ys => f(y) :: ys.myMap(f)

  def myFilter(f: T => Boolean): List[T] = xs match
    case Nil => xs
    case x :: xs =>
      if f(x) then x :: xs.myFilter(f)
      else xs.myFilter(f)

  def pack: List[List[T]] = xs match
    case Nil => Nil
    case x :: xs =>
      val (more, rest) = xs.span(y => y == x)
      (x :: more) :: rest.pack

  def encode: List[(T, Int)] =
    xs.pack.myMap(x => (x.head, x.length))

  def mergeSort(comp: (T, T) => Boolean): List[T] =
    val n = xs.length / 2
    if n == 0 then xs
    else
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if comp(x, y) then x :: merge(xs1, ys)
          else y :: merge(xs, ys1)

      val (first, second) = xs.splitAt(n)
      merge(first.mergeSort(comp), second.mergeSort(comp))
