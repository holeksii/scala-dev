package funsets

object Main extends App:
  import FunSets.*
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)

  val d1 = diff(s1, s1)(1)
  println(d1)
  val d2 = diff(s1, s2)(1)
  println(d2)
