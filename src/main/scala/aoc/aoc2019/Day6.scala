package aoc.aoc2019

import aoc.readFile1

import scala.annotation.tailrec

object Day6 {
  val orbits = readFile1("aoc2019/Day6.txt").map(_.split(')')).map(a => a(1) -> a(0)).toMap

  def pathLength(k: String): Int = {
    @tailrec
    def helper(k: String, acc: Int): Int =
      if (k == "COM") acc else helper(orbits(k), acc + 1)
    helper(k, 0)
  }

  def path(k: String): Seq[String] = {
    @tailrec
    def helper(k: String, acc: Seq[String]): Seq[String] =
      if (k == "COM") acc else helper(orbits(k), acc :+ k)
    helper(k, Seq())
  }

  def part1 = orbits.keySet.toSeq.map(pathLength).sum // 204521

  def part2 = {
    val youPath = path(orbits("YOU")).toSet
    val sanPath = path(orbits("SAN")).toSet
    (youPath.diff(sanPath) ++ sanPath.diff(youPath)).size // 307
  }
}
