package aoc.aoc2021

import aoc._

object Day5 {
  val input = readFile1("aoc2021/Day5.txt").map{case re"(\d+)$x1,(\d+)$y1 -> (\d+)$x2,(\d+)$y2" => Segment(x1.toInt, y1.toInt, x2.toInt, y2.toInt)}.toSeq

  def part1 = {
    val lines = input.filter(_.isHorzOrVert)

//    lines.zip(lines.tails.drop(1)).flatMap{
//      case (l, ls) => ls.map(x => x.overlap(l))
//    }.collect{case Some(t) => t}.flatten.toSet.size //7674

    lines.zip(lines.tails.drop(1)).flatMap{
      case (l, ls) => ls.map(x => x.intersect(l))
    }.filterNot(_.isEmpty).flatten.toSet.size  //7674
  }

  def part2 = {
    input.zip(input.tails.drop(1)).flatMap{
      case (l, ls) => ls.map(x => x.intersect(l))
    }.filterNot(_.isEmpty).flatten.toSet.size  //7674

  }
}
