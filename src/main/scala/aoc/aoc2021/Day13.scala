package aoc.aoc2021

import aoc._

object Day13 {
  val input = readFile1("aoc2021/Day13.txt")
  val coords = input.takeWhile(!_.isEmpty).map { case re"(\d+)$x,(\d+)$y" => (x.toInt, y.toInt) }.toSet
  val instructions = input.map {
    case re"fold along x=(\d+)$x" => foldX(x.toInt)
    case re"fold along y=(\d+)$y" => foldY(y.toInt)
  }.toSeq

  def foldY(y: Int)(p: (Int, Int)) = {
    if (p._2 > y)
      (p._1, y - (p._2 - y))
    else p
  }

  def foldX(x: Int)(p: (Int, Int)) = {
    if (p._1 > x)
      (x - (p._1 - x), p._2)
    else p
  }

  def display(c: Set[(Int, Int)]) = {
    val my = c.map(x => x._1).max
    val mx = c.map(x => x._2).max
    val board = Array.fill(mx+1, my+1)(' ')
    c.foreach(p => board(p._2)(p._1) = '#')
    for (x <- board.indices) {
      for (y <- board(0).indices) {
        print(board(x)(y))
      }
      println
    }
  }

  def part1 = {
    println(coords.size)
    coords.map(c => instructions.head(c)).size //785
  }

  def part2 = {
    display(coords.map(Function.chain(instructions))) //FJAHJGAH
  }

}
