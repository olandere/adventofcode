package aoc.aoc2021

import aoc._

object Day2 {

  val instructions = readFile1("aoc2021/Day2.txt")

  def computePosition(pos: (Int, Int), dir: String) = {
    dir match {
      case re"forward (\d+)$v" => (pos._1 + v.toInt, pos._2)
      case re"up (\d+)$v" => (pos._1, pos._2 - v.toInt)
      case re"down (\d+)$v" => (pos._1, pos._2 + v.toInt)
    }
  }

  def computePosition(pos: (Int, Int, Int), dir: String) = {
    dir match {
      case re"forward (\d+)$v" => (pos._1 + v.toInt, pos._2 + v.toInt * pos._3, pos._3)
      case re"up (\d+)$v" => (pos._1, pos._2, pos._3 - v.toInt)
      case re"down (\d+)$v" => (pos._1, pos._2, pos._3 + v.toInt)
    }
  }

  def part1: Int = {
    val (h, v) = instructions.foldLeft((0, 0))(computePosition)
    h * v //2187380
  }

  def part2: Int = {
    val (h, v, _) = instructions.foldLeft((0, 0, 0))(computePosition)
    h * v //2086357770
  }
}
