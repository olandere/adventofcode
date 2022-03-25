package aoc.aoc2021

import aoc.*

import scala.annotation.tailrec

object Day12 {
  val caves = readFile1("aoc2021/Day12.txt").flatMap{
    case re"(\w+)$a-(\w+)$b" => Seq((Cave(a), Cave(b)), (Cave(b), Cave(a)))
  }.toSeq.groupBy(_._1).map(e => e._1 -> e._2.map(_._2))

  val start = caves.find(_._1.isStart).map(_._1).get

  case class Cave(name: String)  {
    def isLarge = name(0).isUpper

    def isSmall = !isLarge

    def isEnd = name == "end"

    def isStart = name == "start"
  }

  def isAllowed(c: Cave, path: Seq[Cave]): Boolean = c.isLarge || !path.contains(c)

  def isAllowedTwoSmalls(c: Cave, path: Seq[Cave]): Boolean = {
    c.isLarge || (!c.isStart && {
      val smalls = path.filter(x => x.isSmall)
      !path.contains(c) || smalls.size == smalls.toSet.size
    })
  }

  def expand(allowed: (Cave, Seq[Cave]) => Boolean)(path: Seq[Cave]) = {
    caves(path.head).collect{case c if (allowed(c, path)) => c +: path}
  }

  def buildPaths(c: Cave, paths: Seq[Cave], allowed: (Cave, Seq[Cave]) => Boolean): Seq[Seq[Cave]] = {
    @tailrec
    def helper(ps: Seq[Seq[Cave]], complete: Seq[Seq[Cave]]): Seq[Seq[Cave]] = {
      if (ps.isEmpty) complete else {
        val newPaths = ps.flatMap(expand(allowed))
        helper(newPaths.filterNot(_.head.isEnd), complete ++ newPaths.filter(_.head.isEnd))
      }
    }
    helper(Seq(Seq(c)), Seq())
  }

  def part1 = {
    buildPaths(start, Seq(), isAllowed).size
  }

  def part2 = {
    buildPaths(start, Seq(), isAllowedTwoSmalls).size
  }
}
