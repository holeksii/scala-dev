val tolerance = 1e-4

def isCloseEnough(x: Double, y: Double): Boolean =
  Math.abs((x - y) / x) < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
  def iterate(guess: Double): Double =
    val next = f(guess)
    if isCloseEnough(guess, next) then next
    else iterate(next)
  iterate(firstGuess)

def averageDamp(f: Double => Double)(x: Double): Double =
  (x + f(x)) / 2

def sqrt(x: Double): Double = fixedPoint(averageDamp(y => x / y))(1.0)

Math.sqrt(7)
sqrt(7)
