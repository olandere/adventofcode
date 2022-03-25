package aoc.aoc2020

import scala.annotation.tailrec

object Day1 {

  import aoc._

  val expenseReport: Seq[Int] = readFile("aoc2020/Day1.txt").toSeq.map {
    _.toInt
  }.sorted

  def part1: Int = {
    @scala.annotation.tailrec
    def findPair(sum: Int, as: Iterable[Int], bs: Iterable[Int]): (Int, Int) = {
      val a = as.head
      val b = bs.head
      if (a + b == sum) (a, b)
      else if (a + b < sum) findPair(sum, as.tail, bs)
      else findPair(sum, as, bs.tail)
    }

    val (a, b) = findPair(2020, expenseReport, expenseReport.reverseIterator.to(Iterable))
    //println(s"($a, $b)")
    a * b
  }

  def part2: Int = {

    // filter out values too big to be included in solution
    val er = expenseReport.reverseIterator.to(Iterable).dropWhile(2020 - _ < expenseReport.head).toList
    val ers = er.toSet

    @tailrec
    def findTriple(sum: Int, as: Iterable[Int], bs: Iterable[Int]): (Int, Int, Int) = {
      val a = as.head
      val b = bs.head
      if (ers(sum - (a + b))) (a, b, sum - (a + b)) else if (sum - (a + b) < a) findTriple(sum, as, bs.tail) else
        findTriple(sum, as.tail, bs)
    }

    val (a, b, c) = findTriple(2020, expenseReport, er)
    //println(s"($a, $b, $c)")
    a * b * c
  }

}
