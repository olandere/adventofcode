package aoc.aoc2020

object Day3 {

  import aoc._

  val board: Array[String] = readFile("aoc2020/Day3.txt").toArray

  class SlopeIterator(dx: Int, dy: Int) extends Iterator[Char] {
    var sx = 0
    var sy = 0

    override def hasNext: Boolean = sy < board.length

    override def next(): Char = {
      val res = board(sy)(sx)
//      println(s"$sy, $sx, $res")
      sx = (sx + dx) % board(0).length
      sy += dy
      res
    }
  }

  def isTree(ch: Char): Boolean = ch == '#'

  def part1: Int = {
    val iter = new SlopeIterator(3, 1)
    iter.count(isTree)
  }

  def part2: Int = {
    val cnt1 = new SlopeIterator(1, 1).count(isTree)
    val cnt2 = new SlopeIterator(3, 1).count(isTree)
    val cnt3 = new SlopeIterator(5, 1).count(isTree)
    val cnt4 = new SlopeIterator(7, 1).count(isTree)
    val cnt5 = new SlopeIterator(1, 2).count(isTree)
    cnt1 * cnt2 * cnt3 * cnt4 * cnt5
  }
}
