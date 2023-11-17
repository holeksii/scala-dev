trait Generator[+T]:
  def generate(): T

  def map[S](f: T => S) = new Generator[S]:
    def generate() = f(Generator.this.generate())

  def flatMap[S](f: T => Generator[S]) = new Generator[S]:
    def generate() = f(Generator.this.generate()).generate()

val integers = new Generator[Int]:
  val rand = java.util.Random()
  def generate() = rand.nextInt()

val booleans = integers.map(_ > 0)

def single[T](x: T): Generator[T] = new Generator[T]:
  def generate() = x

def pairs[T, U](t: Generator[T], u: Generator[U]) =
  t.flatMap(x => u.map(y => (x, y)))

def range(low: Int, high: Int): Generator[Int] =
  integers.map(low + _.abs % (high - low))

def oneOf[T](xs: T*): Generator[T] =
  for idx <- range(0, xs.length) yield xs(idx)

def lists: Generator[List[Int]] =
  for
    isEmpty <- booleans
    list <- if isEmpty then emptyList else nonEmtpyList
  yield list

def lists(n: Int): Generator[List[Int]] = new Generator[List[Int]]:
  def generate() = (for _ <- 0 until n yield integers.generate()).toList

def lists(minSize: Int, maxSize: Int): Generator[List[Int]] =
  new Generator[List[Int]]:
    def generate() = (for _ <- 0 until range(minSize, maxSize + 1).generate()
    yield integers.generate()).toList

def emptyList = single(Nil)
def nonEmtpyList =
  for
    head <- integers
    tail <- lists
  yield head :: tail

enum Tree:
  case Inner(left: Tree, right: Tree)
  case Leaf(x: Int)

def trees: Generator[Tree] =
  for
    isLeaf <- booleans
    tree <- if isLeaf then leafs else inners
  yield tree

val leafs = for x <- integers yield Tree.Leaf(x)
val inners =
  for
    l <- trees
    r <- trees
  yield Tree.Inner(l, r)

trees.generate()
