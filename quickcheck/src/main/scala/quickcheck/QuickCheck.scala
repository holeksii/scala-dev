package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.util.Try

/** In src/main/scala/quickcheck/QuickCheck.scala, write some more properties
  * that should be satisfied. Your properties should at least cover the
  * following relevant facts:
  *
  * If you insert any two elements into an empty heap, finding the minimum of
  * the resulting heap should get the smallest of the two elements back.
  *
  * If you insert an element into an empty heap, then delete the minimum, the
  * resulting heap should be empty.
  *
  * Given any heap, you should get a sorted sequence of elements when
  * continually finding and deleting minima. (Hint: recursion and helper
  * functions are your friends.)
  *
  * Finding a minimum of the melding of any two heaps should return a minimum of
  * one or the other.
  */
abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      item <- arbitrary[A]
      others <- oneOf(const(empty), genHeap)
    } yield insert(item, others)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if a < b then a else b)
  }

  property("empty") = forAll { (a: A) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorted") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if isEmpty(h) then true
      else {
        val min = findMin(h)
        val rest = deleteMin(h)
        isEmpty(rest) || (min <= findMin(rest) && isSorted(rest))
      }
    isSorted(h)
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    if isEmpty(h1) && isEmpty(h2) then true
    else
      val melded = meld(h1, h2)
      if isEmpty(h1) then findMin(melded) == findMin(h2)
      else if isEmpty(h2) then findMin(melded) == findMin(h1)
      else
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        findMin(melded) == (if min1 < min2 then min1 else min2)
  }

  property("preserveAfterInsertAndDeleteMin") = forAll { (h: H, a: A) =>
    def heapToList(h: H): List[A] =
      if isEmpty(h) then List.empty
      else findMin(h) :: heapToList(deleteMin(h))

    val h1 = insert(a, h)
    val h2 = deleteMin(h1)
    val original = heapToList(h).sorted
    val afterOps = heapToList(h2).sorted

    (original == afterOps) || (original.diff(afterOps).size == 1 && afterOps
      .diff(original)
      .isEmpty)
  }
  // minimum of the melding of any two heaps should return a minimum of one or the other.
  // property("meld2") = forAll { (h1: H, h2: H) =>
  //   val melded = meld(h1, h2)
  //   val min1 = findMin(h1)
  //   val min2 = findMin(h2)
  //   val minMelded = findMin(melded)
  //   minMelded == min1 || minMelded == min2
  // }
