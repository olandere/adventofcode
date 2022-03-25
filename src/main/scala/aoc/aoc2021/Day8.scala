package aoc.aoc2021

import aoc.readFile1

object Day8 {
  val input = readFile1("aoc2021/Day8.txt").toSeq

  def part1 = {
    input.map(_.split('|')(1).trim.split(' ').count(s => Set(2, 3, 4, 7).contains(s.length))).sum //381
  }

  def part2 = {
    input.map{s =>
      val data = s.split('|')
      data(1).trim.split(' ').map(_.toSet).map(mapping(data(0).split(' ').toSeq))
    }.map(_.mkString.toInt).sum //1023686
  }

  def mapping(signals: Seq[String]) = {
    def sameSegments(s1: String, s2: String) = {
      val s2s = s2.toSet
      s1.toSet.intersect(s2s) == s2s
    }

    val m = Map(1 -> signals.find(_.length == 2).getOrElse(""),
      7 -> signals.find(_.length == 3).getOrElse(""),
      4 -> signals.find(_.length == 4).getOrElse(""),
      8 -> signals.find(_.length == 7).getOrElse("")
    )
    val m39 = Map(9 -> signals.find(s => s.length == 6 && sameSegments(s, m(4))).getOrElse(""),
      3 -> signals.find(s => s.length == 5 && sameSegments(s, m(7))).getOrElse(""))
    val m5 = Map(5 -> signals.find(s => s.length == 5 && s != m39(3) && sameSegments(m39(9), s)).getOrElse(""))
    val m260 = Map(2 -> signals.find(s => s.length == 5 && s != m39(3) && s != m5(5)).getOrElse(""),
      6 -> signals.find(s => s.length == 6 && !sameSegments(s, m(7))).getOrElse(""),
      0 -> signals.find(s => s.length == 6 && sameSegments(s, m(7)) && s != m39(9)).getOrElse(""))
    (m ++ m39 ++ m5 ++ m260).map{case (k, v) => v.toSet -> k}
  }
}
