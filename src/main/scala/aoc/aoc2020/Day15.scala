package aoc.aoc2020

import scala.annotation.tailrec

object Day15 {
  import aoc._

  import scala.collection.mutable

  val start = Seq(0,1,5,10,3,12,19)
  //val start = Seq(3,1,2)

  @tailrec
  def helper(prev: Int, next: Int, cnt: Int, end: Int, numbers: mutable.Map[Int, Int]): Int = {
    //println(s"Turn $cnt: $next")
    if (cnt == end) next else

      if (!numbers.contains(next)) {
        numbers(next) = cnt
        helper(next, 0, cnt+1, end, numbers)
      } else {
        val pe = numbers(next)
        numbers(next) = cnt
        helper(next, cnt - pe, cnt+1, end, numbers)
      }
  }

  def part1: Int = {
    // idea - keep a map of values and last index, update map with new index when value seen again.
    val numbers: mutable.Map[Int, Int] = mutable.HashMap.from(start.zipWithIndex.map(e => (e._1, e._2+1)))

    helper(start.last, 0, numbers(start.last)+1, 2020, numbers) //1373
  }

  def part2: Int = {
    val numbers: mutable.Map[Int, Int] = mutable.HashMap.from(start.zipWithIndex.map(e => (e._1, e._2+1)))

    helper(start.last, 0, numbers(start.last)+1, 30000000, numbers)
  }
}
