package parallelprog

def mapArrSeg[A, B](
    in: Array[A],
    left: Int,
    right: Int,
    f: A => B,
    out: Array[B]
): Unit =
  var i = left
  while i < right do
    out(i) = f(in(i))
    i += 1

def mapArrayPar[A, B](
    in: Array[A],
    f: A => B,
    numOfTasks: Int,
    out: Array[B]
): Unit =
  if numOfTasks < 1 then throw new IllegalArgumentException("numOfTasks < 1")
  else if numOfTasks == 1 then mapArrSeg(in, 0, in.length, f, out)
  else
    val treshold = in.length / numOfTasks

    def mapFromLeftToRight(left: Int, right: Int): Unit =
      if right - left < treshold then mapArrSeg(in, left, right, f, out)
      else
        val mid = left + (right - left) / 2
        parallel(
          mapFromLeftToRight(left, mid),
          mapFromLeftToRight(mid, right)
        )

    mapFromLeftToRight(0, in.length)

def reduceSegPar[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A =
  if right - left < threshold then
    var res = inp(left); var i = left + 1
    while (i < right) { res = f(res, inp(i)); i = i + 1 }
    res
  else
    val mid = left + (right - left) / 2
    val (a1, a2) =
      parallel(
        reduceSegPar(inp, left, mid, f),
        reduceSegPar(inp, mid, right, f)
      )
    f(a1, a2)

def reducePar[A](inp: Array[A], f: (A, A) => A): A =
  reduceSegPar(inp, 0, inp.length, f)

def scanLeft[A](a0: A)(inp: Array[A], f: (A, A) => A, out: Array[A]): Unit =
  out(0) = a0
  var a = a0
  var i = 0
  while i < inp.length do
    a = f(a, inp(i))
    i = i + 1
    out(i) = a
