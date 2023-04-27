package example

import java.{util => ju, lang => jl}

object Lists:

  /** This method computes the sum of all elements in the list xs. There are
    * multiple techniques that can be used for implementing this method, and you
    * will learn during the class.
    *
    * For this example assignment you can use the following methods in class
    * `List`:
    *
    *   - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
    *   - `xs.head: Int` returns the head element of the list `xs`. If the list
    *     is empty an exception is thrown
    *   - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
    *     list `xs` without its `head` element
    *
    * ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
    * solution.
    *
    * @param xs
    *   A list of natural numbers
    * @return
    *   The sum of all elements in `xs`
    */
  def sum(xs: List[Int]): Int = xs.sum

  /** This method returns the largest element in a list of integers. If the list
    * `xs` is empty it throws a `java.util.NoSuchElementException`.
    *
    * You can use the same methods of the class `List` as mentioned above.
    *
    * ''Hint:'' Again, think of a recursive solution instead of using looping
    * constructs. You might need to define an auxiliary method.
    *
    * @param xs
    *   A list of natural numbers
    * @return
    *   The largest element in `xs`
    * @throws java.util.NoSuchElementException
    *   if `xs` is an empty list
    */
  def max(xs: List[Int]): Int =
    if !xs.isEmpty then xs.max else throw ju.NoSuchElementException()

  def sqrt(x: Double): Double =
    def sqrtIter(guess: Double): Double =
      if isGoodEnought(guess) then guess
      else sqrtIter(improve(guess))

    def improve(guess: Double) =
      (guess + x / guess) / 2

    def isGoodEnought(guess: Double) =
      jl.Math.abs(guess * guess - x) < 0.0000001

    sqrtIter(1)

  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  def factorial(n: Int): Int =
    if n == 0 then 1 else n * factorial(n - 1)

@main
def run =
  println("-" * 100)

  val v = Lists.gcd(80, 45)
  println(v)

  println("-" * 100)
