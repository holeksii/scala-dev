package recfun

import scala.annotation.tailrec
import java.util.Calendar

object RecFun extends RecFunInterface:

  def factorial(n: Int): Int =
    if n == 0 then 1
    else n * factorial(n - 1)

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
    def isOpenBracket(bracket: Char): Boolean = bracket == '('

    def isCloseBracket(bracket: Char): Boolean = bracket == ')'

    @tailrec
    def iter(chars: List[Char], openBraces: Int): Boolean =
      if openBraces < 0 then false
      else if chars.isEmpty then openBraces == 0
      else if isOpenBracket(chars.head) then iter(chars.tail, openBraces + 1)
      else if isCloseBracket(chars.head) then iter(chars.tail, openBraces - 1)
      else iter(chars.tail, openBraces)

    iter(chars, 0)

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then 1
    else if money < 0 || coins.isEmpty then 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
