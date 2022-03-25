package aoc.aoc2021

import aoc._

object Day7 {
  val input: Seq[Int] = readFile1("aoc2021/Day7.txt").next().split(',').map(_.toInt).toSeq

  def cost(crab: Int, dest: Int) = Math.abs(crab - dest)

  def cost2(crab: Int, dest: Int) = {
    val x = Math.abs(crab - dest)
    x * (x + 1) / 2
  }

  def totalFuel(cs: Iterable[Int], c: (Int, Int) => Int, dest: Int) =
    cs.map(x => c(x, dest)).sum

  def bounds(c: (Int, Int) => Int)(lb: Int, ub: Int): (Int, Int) = {
    val a = totalFuel(input, c, lb)
    val b = totalFuel(input, c, ub)
    println(s"$lb, $ub, $a, $b")
    val mid = (lb + ub) / 2
    if (a < b) (lb, mid) else (mid, ub)
  }

  def part1 = {
    val (a, b) = iterate(input.minMax, bounds(cost).tupled, (x) => Math.abs(x._1 - x._2) < 2)
    Math.min(totalFuel(input, cost, a), totalFuel(input, cost, b)) //    348996
  }

  def part2 = {
    val (a, b) = iterate(input.minMax, bounds(cost2).tupled, (x) => Math.abs(x._1 - x._2) < 2)
    Math.min(totalFuel(input, cost2, a), totalFuel(input, cost2, b)) //    348996
  }
}
