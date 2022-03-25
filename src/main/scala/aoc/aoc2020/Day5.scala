package aoc.aoc2020

import scala.collection.immutable.Range.Inclusive
import scala.io.Source

object Day5 {
  import aoc._

  val passes: Iterator[String] = readFile("aoc2020/Day5.txt")

  def bsp(r: Inclusive, ch: Char): Inclusive = {
    val mid = r.start + (r.end - r.start + 1)/2 - 1
    ch match {
      case 'F' | 'L' => r.start to mid
      case 'B' | 'R' => mid + 1 to r.end
    }
  }

  val seatIds: Seq[Int] = passes.map{ l =>
    val (row, seat) = l.splitAt(7)
    val r = row.foldLeft(0 to 127)(bsp).head
    val s = seat.foldLeft(0 to 7)(bsp).head
    r*8+s
  }.toSeq

  def part1: Int = {
    seatIds.max
  }

  def part2: Int = {
    val sortedIds = seatIds.sorted
    sortedIds.zip(sortedIds.tail).filter{case (a, b) => (b - a) == 2}.head._1+1
  }
}
