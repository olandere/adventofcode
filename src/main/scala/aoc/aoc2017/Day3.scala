package aoc.aoc2017

import scala.annotation.tailrec

object Day3 {
  val start = 265149

  def nearestOddSqrt(n: Int): Int = {
    val sqrt = math.sqrt(n).toInt
    if (sqrt % 2 == 0) sqrt - 1 else sqrt
  }

  def genCoords(path: String, v: Int, x: Int, y: Int): Seq[(Int, Int, Int)] = {
    @tailrec
    def helper(path: Array[Char], v: Int, x: Int, y: Int, acc:Seq[(Int, Int, Int)]): Seq[(Int, Int, Int)] = {
      if (path.isEmpty) acc else {
        val next = path.head match {
          case 'N' => (v + 1, x, y + 1)
          case 'S' => (v + 1, x, y - 1)
          case 'E' => (v + 1, x - 1, y)
          case 'W' => (v + 1, x + 1, y)
        }
        helper(path.tail, next._1, next._2, next._3, acc:+next)
      }
    }

    helper(path.toCharArray, v, x, y, Seq())
  }

  def spiral(st: Int, x: Int, y: Int): Seq[(Int, Int, Int)] = {
    val n = st + 2
    val path = "W"+"N".repeat(n-2)+"E".repeat(n-1)+"S".repeat(n-1)+"W".repeat(n-1)
    genCoords(path, st*st, x, y)
  }

  def part1: Option[Int] = {
    //idea - if you look at the spiral pattern, notice that the values along the SE diagonal are squares of
    //consecutive odd numbers
    val sqrt = nearestOddSqrt(start)

    // also, the values leading up to the next sqrt in the spiral are all within a bound
    val bound = sqrt/2
    spiral(sqrt, bound, -bound).find(_._1 == start).map{p => math.abs(p._2)+math.abs(p._3)}
  }

  def part2: Unit = {

  }
}
