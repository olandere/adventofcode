package aoc.aoc2019

import aoc._

object Day1 {
  val input = readFile1("aoc2019/Day1.txt").map(_.toInt).toSeq

  def fuel(m: Int): Int = m/3 - 2

  def totalFuel(m: Int) = {
    def helper(m: Int, acc: Long): Long = {
      if (m <= 0) acc else
        helper(fuel(m), acc+m)
    }
    helper(fuel(m), 0L)
  }

  val part1 = input.map(fuel).sum // 3235550

  def part2 = {
    input.map(totalFuel).sum // 4850462
  }
}
