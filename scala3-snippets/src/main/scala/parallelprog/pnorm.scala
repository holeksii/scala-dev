package parallelprog

def powFloor(x: Double, p: Double) = math.exp(p * math.log(x)).toInt

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int =
  var i = s; var sum: Int = 0
  while (i < t) {
    sum = sum + powFloor(a(i).abs, p)
    i = i + 1
  }
  sum

def pNormRec(a: Array[Int], p: Double) =
  powFloor(segmentRec(a, p, 0, a.length), 1 / p)

val threshold = 100

def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Double =
  if t - s < threshold then sumSegment(a, p, s, t)
  else
    val m = s + (t - s) / 2
    val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t))
    sum1 + sum2
