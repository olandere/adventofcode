package aoc.aoc2015

import scala.annotation.tailrec

object Day1 {
  import aoc._

  val input: String = readFile("aoc2015/Day1.txt").next()

  def convert(ch: Char): Int = ch match {case '(' => 1; case ')' => -1}

  def part1: Int = {
    input.map{convert}.sum //232
  }

  def part2: Int = {
    @tailrec
    def helper(xs: Seq[Int], acc: Int = 0, pos: Int = 0): Int = {
      if (acc < 0) pos
      else helper(xs.tail, acc + xs.head, pos+1)
    }

    helper(input.map{convert})
  }
}
