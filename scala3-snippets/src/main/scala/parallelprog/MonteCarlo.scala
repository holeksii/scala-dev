package parallelprog

object MonteCarlo:
  import scala.util.Random

  private def mcCount(iter: Int): Int =
    val randX = Random
    val randY = Random
    var hits = 0

    for i <- 0 until iter do
      val x = randX.nextDouble()
      val y = randY.nextDouble()
      if x * x + y * y <= 1 then hits += 1

    hits

  def piEstSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter
  def piEstPar(iter: Int): Double =
    val (pi1, pi2) = parallel(mcCount(iter / 2), mcCount(iter - iter / 2))
    4.0 * (pi1 + pi2) / iter
