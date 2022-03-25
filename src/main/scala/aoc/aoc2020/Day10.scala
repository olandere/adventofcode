package aoc.aoc2020

import scala.annotation.tailrec

object Day10 {
  import aoc._

  val adapters: Seq[String] = readFile("aoc2020/Day10.txt").toSeq

  def part1: Int = {
    val sa = adapters.map(_.toInt)
    val sam = (sa :+ sa.max + 3 :+ 0).sorted
    val counts = sam.zip(sam.tail).map(x => x._2 - x._1).groupBy(x=>x).map{case(k, v) => k -> v.size}
    counts(1)*counts(3) //2272
  }

  def part2a: Int = {
    val sa = Seq(1,2,3,4) //adapters.map(_.toInt).toSeq
    val sam = (sa :+ sa.max + 3 :+ 0).sorted

    def count(h: Seq[Int], t: Seq[Int], br: String): Int = {
      //println(s"($br) h: $h, t: $t")

      def isAllowed(a: Int, b: Int) = (b - a) < 4

      if (t.tail.nonEmpty)
        if (isAllowed(h.head, t.tail.head)) {
          count(t.head +: h, t.tail, br+"L") + count(h, t.tail, br+"R")
        } else count(t.head +: h, t.tail, br)
      else 1
    }

    count(Seq(sam.head), sam.tail, "")

    //84627647627264
  }

  /**
   * Part2a correctly computes the result but is prohibitive in time.  Exploring the behavior of the values and
   * realizing that only strings where the diff in the values is 1 matter (the 3s can act as delimiters) then
   * just the results of computing the combinations of digits where the difference is 1 and multiplying those
   * together would yield the total number of combinations.  Also, it turns out that the number of combinations
   * follows the Tribonacci sequence.
   */
  def part2: Long = {
    /**
     * Compute the nth Tribonacci number (only use for small n)
     * @param n
     * @return
     */
    def trib(n: Int): Long =
      n match {
        case 0 => 0L
        case 1 => 1L
        case 2 => 1L
        case _ => trib(n-1) + trib(n-2) + trib(n-3)
      }

    /**
     * run length encode a seq of ints.
     * @param xs
     * @return
     */
    def rle(xs: Seq[Int]): Seq[(Int, Int)] = {
      @tailrec
      def helper(currVal: Int, rest: Seq[Int], cnt: Int, acc: Seq[(Int, Int)]): Seq[(Int, Int)] = {
        if (rest.isEmpty) acc else if (currVal == rest.head) helper(currVal, rest.tail, cnt + 1, acc) else {
          helper(rest.head, rest.tail, 1, acc :+ (currVal, cnt))
        }
      }
      helper(xs.head, xs.tail, 1, Seq.empty)
    }

    val sa = adapters.map(_.toInt)
    val sam = (sa :+ sa.max + 3 :+ 0).sorted
    rle(sam.zip(sam.tail).map(x => x._2 - x._1)).collect{case(1, x) => trib(x+1)}.product
  }
}
