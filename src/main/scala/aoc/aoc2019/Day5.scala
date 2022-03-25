package aoc.aoc2019

import aoc.readFile1

object Day5 {
  val intcode = readFile1("aoc2019/Day5.txt").next().split(',').map(_.toInt).toSeq

  def part1 = {
    val code = intcode.toArray
    Intcode().run(code, 0) // 5074395
  }

  def part2  = {
    val code = intcode.toArray
    def ic = Intcode()
    ic.input.enqueue(5)
    ic.run(code, 0)
    ic.output
    // 8346937
  }
}
