package aoc.aoc2021

import aoc._

object Day10 {
  val input: Seq[String] = readFile1("aoc2021/Day10.txt").toSeq

  val expected = Map('{' -> '}', '[' -> ']', '<' -> '>', '(' -> ')')
  val closing: Set[Char] = Set('}', ']', '>', ')')
  val score = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val part2score = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  def parseString(str: String) = {
    def helper(stack: String, str: String): Either[Char, String] = {
      if (str.isEmpty) Right(stack)
      else {
        if (closing(str.head))
          if (expected(stack.head) == str.head) helper(stack.tail, str.tail) else Left(str.head)
        else helper(str.head + stack, str.tail)
      }
    }

    helper(str.head.toString, str.tail)
  }

  def part1 = {
    input.map(parseString).collect { case Left(c) => score(c) }.sum
  }

  def part2 = {
    val scores = input.map(parseString).collect{case Right(s) => s.map(expected).foldLeft(0L)((i, ch) => i*5+part2score(ch))}.sorted
    scores.drop(scores.length/2).head
  }
}
