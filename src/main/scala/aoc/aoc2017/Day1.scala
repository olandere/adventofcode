package aoc.aoc2017

import scala.annotation.tailrec

object Day1 {

  import aoc._

  val input: Seq[Int] = toIntSeq(readFile1("aoc2017/Day1.txt").next())

  def toIntSeq(str: String): Seq[Int] = str.toCharArray.map(_.asDigit)

  def circSum(xs: Seq[Int]): Int = {
    @tailrec
    def helper(f: Int, h: Int, t: Seq[Int], cnt: Int): Int = {
      if (t.isEmpty) {
        if (f == h) cnt + f else cnt
      } else {
        if (h == t.head) helper(f, t.head, t.tail, cnt + h)
        else helper(f, t.head, t.tail, cnt)
      }
    }

    helper(xs.head, xs.head, xs.tail, 0)
  }

  def splitZip(s: Seq[Int]): Seq[(Int, Int)] = {
    val mid = s.length / 2
    val (a, b) = s.splitAt(mid)
    a.zip(b)
  }

  def part1: Int = circSum(input) //1223

  def part2: Int = splitZip(input).collect{case (a, b) if a == b => a+b}.sum //1284
}
