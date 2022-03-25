package aoc.aoc2021

import aoc._

object Day3 {

  val codes = readFile1("aoc2021/Day3.txt").toSeq

  def stringToIntSeq(s: String): Seq[Int] = s.map(_.asDigit)

  def addIntSeq(s1: Seq[Int], s2: Seq[Int]): Seq[Int] = s1.zip(s2).map(e => e._1 + e._2)

  def complement(s: Seq[Int]): Seq[Int] = s.map(v => if (v == 1) 0 else 1)

  def binSeqToInt(s: Seq[Int]): Int = Integer.parseInt(s.mkString, 2)

  def mostFrequent(c: Seq[String]): Seq[Int] = {
    val mid = c.length / 2 + (c.length & 1) // round up for odd lengths
    val zero = stringToIntSeq("0" * c.head.length)
    c.map(stringToIntSeq).foldLeft(zero)(addIntSeq).map(v => if (v >= mid) 1 else 0)
  }

  def part1 = {
    val g = mostFrequent(codes)
    val e = complement(g)
    binSeqToInt(g) * binSeqToInt(e) // 2972336
  }

  def part2 = {
    def helper(pos: Int, cs: Seq[String], p: Boolean => Boolean): Seq[String] = {
      if (pos == cs.head.length || cs.sizeIs < 2) cs else {
        val freq = mostFrequent(cs).mkString
        helper(pos + 1, cs.filter(c => p(c(pos) == freq(pos))), p)
      }
    }

    val intVal = stringToIntSeq.andThen(binSeqToInt)
    val oxygenGeneratorRating = intVal(helper(0, codes, (x: Boolean) => x).head)
    val co2ScrubberRating = intVal(helper(0, codes, (x: Boolean) => !x).head)
    oxygenGeneratorRating * co2ScrubberRating //3368358
  }
}
