package aoc.aoc2020

object IterateFun {

  import aoc._

  def sqrt(n: Double, delta: Double): Double = {
    def estimate(x: Double): Double = (x + n / x) / 2.0
    def isCloseEnough(x: Double) = math.abs(n - x * x) < delta
    iterate(1.0, estimate, isCloseEnough)
  }

}
