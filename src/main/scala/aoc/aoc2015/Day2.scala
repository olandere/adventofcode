package aoc.aoc2015

object Day2 {

  import aoc._

  val presents: Seq[List[Int]] = readFile("aoc2015/Day2.txt").map{_.split('x').toList.map(_.toInt)}.toSeq

  def surfaceArea(l: Int, w: Int, h: Int): Int = 2*l*w + 2*w*h + 2*h*l

  def volume(l: Int, w: Int, h: Int): Int = l*w*h

  def slack(l: Int, w: Int, h: Int): Int = Seq(l, w, h).sorted.take(2).product

  def totalPaper(l: Int, w: Int, h: Int): Int = surfaceArea(l, w, h) + slack(l, w, h)

  def ribbon(l: Int, w: Int, h: Int): Int = Seq(l, w, h).sorted.take(2).sum*2

  def totalRibbon(l: Int, w: Int, h: Int): Int = ribbon(l, w, h) + volume(l, w, h)

  def part1: Int = {
    presents.map{case List(l, w, h) => totalPaper(l, w, h)}.sum //1588178
  }

  def part2: Int = {
    presents.map{case List(l, w, h) => totalRibbon(l, w, h)}.sum //3783758
  }
}
