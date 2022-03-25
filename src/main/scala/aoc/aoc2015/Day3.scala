package aoc.aoc2015

object Day3 {

  import aoc._

  val directions: String = readFile("aoc2015/Day3.txt").next()

  def nextPos(curr: (Int, Int), ch: Char): (Int, Int) =
    ch match {
      case 'v' => (curr._1, curr._2 - 1)
      case '<' => (curr._1 - 1, curr._2)
      case '>' => (curr._1 + 1, curr._2)
      case '^' => (curr._1, curr._2 + 1)
    }

  def part1: Int = {
    directions.scanLeft((0, 0))((pos, ch) => nextPos(pos, ch)).toSet.size //2081
  }

  def part2: Int = {
    val santa = directions.sliding(1,2).scanLeft((0, 0))((pos, ch) => nextPos(pos, ch.head)).toSet
    val robo = directions.drop(1).sliding(1,2).scanLeft((0, 0))((pos, ch) => nextPos(pos, ch.head)).toSet
    (santa++robo).size //2341
  }
}
