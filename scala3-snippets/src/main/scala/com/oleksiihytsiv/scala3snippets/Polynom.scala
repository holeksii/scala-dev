package com.oleksiihytsiv.scala3snippets

class Polynom(nonZeroTerms: Map[Int, Double]):
  def this(bindings: (Int, Double)*) =
    this(bindings.toMap)

  val terms = nonZeroTerms.withDefaultValue(0.0)

  def `_+`(other: Polynom): Polynom =
    Polynom(terms ++ other.terms.map((x, y) => (x, y + terms(x))))

  /** time complexity: `O(n)`, where n is a count of terms
    *
    * @param other
    * @return
    */
  def +(other: Polynom): Polynom =
    Polynom(other.terms.foldLeft(terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) =
    terms + (term._1 -> (terms(term._1) + term._2))

  override def toString(): String =
    val termStrings =
      for (exp, coeff) <- terms.toList.sorted.reverse
      yield
        val exponent = if exp != 1 then s"x^$exp" else "x"
        val coefficient = if coeff % 1 == 0 then coeff.toInt.toString else coeff.toString
        if exp == 0 then s"$coefficient" else s"$coefficient$exponent"

    if terms.isEmpty then "0" else termStrings.filter(_ != "").mkString(" + ")

@main
def testPolynom =
  val p1 = Polynom(Map[Int, Double](0 -> -3, 1 -> 3.3, 2 -> 1))
  val p2 = Polynom(0 -> 2.2, 1 -> 3.3, 2 -> 1)
  println(p1 + p2)
