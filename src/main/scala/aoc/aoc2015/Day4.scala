package aoc.aoc2015

object Day4 {

  import aoc._
  
  val key = "yzbqklnj"

  def part1: Int = {
    IntSeq.find(i => hasFiveZeros(md5(key, i))).get //282749
  }

  def part2: Int = {
    IntSeq.find(i => hasSixZeros(md5(key, i))).get //9962624
  }
}
