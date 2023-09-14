package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer (Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 10_000
    val chars = new Array[Char](length)
    val threshold = 1_000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing
    extends ParallelParenthesesBalancingInterface:

  def isOpenParentheses(bracket: Char): Boolean = bracket == '('
  def isCloseParentheses(bracket: Char): Boolean = bracket == ')'

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean =
    @tailrec
    def iter(chars: Array[Char], openParentheses: Int): Boolean =
      if openParentheses < 0 then false
      else if chars.isEmpty then openParentheses == 0
      else if isOpenParentheses(chars.head) then
        iter(chars.tail, openParentheses + 1)
      else if isCloseParentheses(chars.head) then
        iter(chars.tail, openParentheses - 1)
      else iter(chars.tail, openParentheses)

    iter(chars, 0)

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) =
      if idx >= until then (arg1, arg2)
      else if isOpenParentheses(chars(idx)) then
        traverse(idx + 1, until, arg1 + 1, arg2)
      else if isCloseParentheses(chars(idx)) then
        if arg1 > 0 then traverse(idx + 1, until, arg1 - 1, arg2)
        else traverse(idx + 1, until, arg1, arg2 + 1)
      else traverse(idx + 1, until, arg1, arg2)

    def reduce(from: Int, until: Int): (Int, Int) =
      if until - from < threshold then traverse(from, until, 0, 0)
      else
        val mid = from + (until - from) / 2
        val (a1, a2) = parallel(reduce(from, mid), reduce(mid, until))
        (a1._1 + a2._1, a1._2 + a2._2)

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

@main def main =
  val n = 1_000
  val str = """(if (zero? x) max (/ 1 x))
I told him (that it's not (yet) done). (But he wasn't listening)""" * n

  println {
    ParallelParenthesesBalancing.balance(str.toCharArray)
  }

  println {
    ParallelParenthesesBalancing.parBalance(str.toCharArray, n)
  }
