package aoc.aoc2020

import scala.annotation.tailrec

object Day9 {
  import aoc._

  val data: Seq[Long] = readFile("aoc2020/Day9.txt").map(_.toLong).toSeq

  def isValid(preamble: Set[Long], num: Long): Boolean = {
    (for {
      i <- preamble if num - i != i && preamble(num - i)
    } yield i).nonEmpty
  }

  def part1: Long = {

    @tailrec
    def findInvalidValue(list: Seq[Long]): Long = {
      if (isValid(list.take(25).toSet, list.drop(25).head)) {
        findInvalidValue(list.tail)
      } else list.drop(25).head
    }

    findInvalidValue(data) // 105950735
  }

  def part2: Long = {
    @tailrec
    def findNums(list: Seq[Long], num: Long): Seq[Long] = {
      @tailrec
      def helper(xs: Seq[Long], acc: Seq[Long], v: Long): Seq[Long] = {
        if (v > num) Seq.empty
        else if (v == num) acc
        else helper(xs.tail, acc :+ xs.head, v + xs.head)
      }

      val res = helper(list, Seq.empty, 0)
      if (res.nonEmpty) res
      else
        findNums(list.tail, num)
    }

    val invalidNum = part1
    val s = findNums(data, invalidNum)
    s.min + s.max // 13826915
  }
}
