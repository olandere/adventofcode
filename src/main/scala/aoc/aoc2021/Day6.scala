package aoc.aoc2021

import aoc._

object Day6 {

  val input: Map[Int, Long] = readFile1("aoc2021/Day6.txt").next().
    split(',').map(_.toInt).groupBy{x => x}.map{case (k, v) => k -> v.size.toLong}.withDefaultValue(0L)

  def process(p: (Map[Int, Long], Int)) = {
    (newMap(p._1), p._2 - 1)
  }

  def newMap(m: Map[Int, Long]): Map[Int, Long] = {
    Map(0 -> m(1), 1 -> m(2), 2 -> m(3), 3 -> m(4), 4 -> m(5), 5 -> m(6), 6 -> (m(7) + m(0)), 7 -> m(8), 8 -> m(0))
  }

  def numDays(p: (_, Int)) = p._2 == 0

  def part1 = {
    val (fish, _) = iterate((input, 80), process, numDays)
    fish.map{case (_, v) => v}.sum //386755
  }

  def part2 = {
    val (fish, _) = iterate((input, 256), process, numDays)
    fish.map{case (_, v) => v}.sum //386755
  }
}
