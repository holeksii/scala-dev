import scala.annotation.tailrec

def linRecSum(f: Int => Int, a: Int, b: Int): Int =
  if a > b then 0
  else f(a) + linRecSum(f, a + 1, b)

def tailRecSum(f: Int => Int, a: Int, b: Int): Int =
  @tailrec
  def loop(a: Int, acc: Int): Int =
    if a > b then acc
    else loop(a + 1, acc + f(a))
  loop(a, 0)

def sum(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 0
  else f(a) + sum(f)(a + 1, b)

def prod(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 1
  else f(a) * prod(f)(a + 1, b)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(
    a: Int,
    b: Int
): Int =
  def recur(a: Int): Int =
    if a > b then zero
    else combine(f(a), recur(a + 1))
  recur(a)

def factorial(x: Int) = mapReduce(x => x, (x, y) => x * y, 1)

def sum(f: Int) = mapReduce(x => x, (x, y) => x + y, 0)
val sumOfCubes = sum(x => x * x * x)(1, 3)

def f(a: String)(b: Int)(c: Boolean): String =
  "(" + a + ", " + b + ", " + c + ")"

val partialApplication1 = f("Scala")

val partialApplication2 = partialApplication1(42)
