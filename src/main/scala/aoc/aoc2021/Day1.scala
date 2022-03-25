package aoc.aoc2021

import aoc._

object Day1 {
  val depths = readFile1("aoc2021/Day1.txt").map(_.toInt).toSeq

  def part1: Int = {
    depths.zip(depths.tail).count(p => p._1 < p._2) //1266
  }

  def part2: Int = {
    val s = depths.sliding(3).map(_.sum).toList
    s.zip(s.tail).count(p => p._1 < p._2)  //1217
  }
}
