package aoc.aoc2019

import aoc.readFile1

object Day2 {
  val intcode = readFile1("aoc2019/Day2.txt").next().split(',').map(_.toInt).toSeq

  def part1 = {
    val code = intcode.toArray
    code(1) = 12
    code(2) = 2
    Intcode().run(code, 0)(0) // 3562672
  }

  def part2 = {
    def helper(i: Int, j: Int): Int = {
      val code = intcode.toArray
      code(1) = i
      code(2) = j
      Intcode().run(code, 0)
      if (code(0) == 19690720) i * 100 + j else {
        if (j == 99) helper(i+1, 0) else helper(i, j+1)
      }
    }
    helper(0, 0) // 8250
  }
}
