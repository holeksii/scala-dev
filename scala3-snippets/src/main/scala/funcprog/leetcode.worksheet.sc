import scala.annotation.tailrec

object RichestCustomerWealth:
  def maximumWealth(accounts: Array[Array[Int]]): Int =
    accounts.map(_.sum).max

object GroupAnagrams:
  def groupAnagrams(strs: Array[String]): List[List[String]] =
    strs.groupBy(_.sorted).values.map(_.toList).toList

object LongestSubstringWithoutRepeatingCharacters:
  def lengthOfLongestSubstring(s: String): Int =
    def loop(s: String, acc: Int, max: Int, seen: Set[Char]): Int =
      if s.isEmpty then max
      else if seen.contains(s.head) then loop(s.tail, 1, max, Set.empty)
      else loop(s.tail, acc + 1, max.max(acc + 1), seen + s.head)
    loop(s, 0, 0, Set.empty)

object MaximumNumberOfBalloons:
  val balloon = "balloon"

  def maxNumberOfBalloons(text: String): Int =
    val charOcc = text
      .groupBy(identity)
      .filter((k, _) => balloon.contains(k))
      .map((k, v) => (k, (v.length / balloon.count(_ == k)).toInt))
    if charOcc.keySet != balloon.toSet then 0
    else charOcc.values.min

  def maxNumberOfBalloonsOneLiner(text: String): Int =
    "balon".zip(List(1, 1, 2, 2, 1)).map((k, v) => text.count(_ == k) / v).min

object MaximumNumberOfStringPairs:
  def isPalindrome(s1: String, s2: String) = s1.equals(s2.reverse)
  def maximumNumberOfStringPairs(words: Array[String]): Int =
    var i = 0
    for
      s1 <- words
      s2 <- words
      if isPalindrome(s1, s2) && !s1.equals(s2)
    do i += 1
    i / 2

object PalindromePairs:
  def isPalindrome(s: String) =
    @tailrec
    def loop(s: String, i: Int, j: Int): Boolean =
      if i >= j then true
      else if s(i) != s(j) then false
      else loop(s, i + 1, j - 1)
    loop(s, 0, s.length - 1)

  def palindromePairs(words: Array[String]): List[List[Int]] =
    (for
      i <- 0 until words.length
      j <- 0 until words.length
      if i != j && isPalindrome(words(i) + words(j))
    yield List(i, j)).toList

import PalindromePairs.palindromePairs
