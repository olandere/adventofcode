package aoc.aoc2017

import scala.annotation.tailrec

object Day2 {

  import aoc._

  val spreadsheet: Seq[Seq[Int]] = buildSheet(readFile1("aoc2017/Day2.txt").toSeq)

  def buildSheet(iter: Seq[String]): Seq[Seq[Int]] = {
    iter.map {
      _.split("\\s+").map(_.toInt).toSeq
    }
  }

  def minMaxDiff(arr: Seq[Int]): Int = {
    val (min, max) = arr.minMax
    max - min
  }

  def divides(a: Int, b: Int): Boolean = if (a < b) b % a == 0 else a % b == 0

  def findQuotient(arr: Seq[Int]): Int = {
    @tailrec
    def helper(h: Int, t: Seq[Int]): Int = {
      val l = for {i <- t if divides(h, i)} yield i
      if (l.isEmpty) helper(t.head, t.tail)
      else {
        val n = l.head
        if (h < n) n / h else h / n
      }
    }

    helper(arr.head, arr.tail)
  }

  def part1: Int = {
    spreadsheet.map(minMaxDiff).sum //30994
  }

  def part2: Int = {
    spreadsheet.map(findQuotient).sum //233
  }
}
