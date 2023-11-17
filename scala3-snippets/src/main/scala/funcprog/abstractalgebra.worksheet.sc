trait SemiGroup[T]:
  extension (x: T) def combine(y: T): T

trait Monoid[T] extends SemiGroup[T]:
  def unit: T

object Monoid:
  def apply[T](using m: Monoid[T]): Monoid[T] = m

def reduce[T: Monoid](xs: List[T]) =
  xs.foldLeft(Monoid[T].unit)(_ combine _)

given sumMonoid: Monoid[Int] with
  extension (x: Int) def combine(y: Int): Int = x + y
  def unit: Int = 0

given prodMonoid: Monoid[Int] with
  extension (x: Int) def combine(y: Int): Int = x * y
  def unit: Int = 1

def sum[T: Monoid](xs: List[T]): T = reduce(xs)

val xs = sum((1 to 100).toList)(using sumMonoid)
