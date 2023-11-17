import scala.util.Random

val n = 100
val xs = List.fill(n)(Random.nextInt)

// Task 1
// Find the last element of List
xs.last
// Find the last element of List using tail recursion
def last[T](xs: List[T]): T = xs match
  case Nil      => throw new NoSuchElementException
  case x :: Nil => x
  case _ :: xs  => last(xs)

last(xs)
// Test
assert(xs.last == last(xs))

// Task 2
// Find 3 next numbers of each element
xs.flatMap(x => List(x, x + 1, x + 2))

// Task 3
def groupBy2Elemets[T](xs: List[T]): List[List[T]] = xs match
  case Nil            => List()
  case y :: Nil       => List(xs)
  case y1 :: y2 :: ys => List(y1, y2) :: groupBy2Elemets(ys)

val range = (2 to n + 1)
groupBy2Elemets(range.toList)

// Task 4
val listOfLists = List(1, 2, 3, List(4, 5, 6, List(7, 8, 9)))
// ys.flatten ???
def flatten(xs: Any): List[Any] = xs match
  case Nil     => Nil
  case y :: ys => flatten(y) ::: flatten(ys)
  case _       => xs :: Nil

flatten(listOfLists)

// Task 5
val char = "*"
(5 to 1 by -1).foreach(x => println(char * x))

// Task 6
// Words count
val names =
  List("Jack Stone", "John Smith", "Emily Stone", "Emily Cooper", "John Cooper")
names flatMap (_.split(" ")) groupBy (identity) map (x => (x._1 -> x._2.size))

// Task 7
// n-th last element
def nthLast[T](xs: List[T], n: Int) =
  val m = xs.size - n - 1

  def iter(xs: List[T], n: Int): T =
    if n == 0 then xs.head
    else iter(xs.tail, n - 1)

  iter(xs, m)

nthLast(flatten(listOfLists), 3)
