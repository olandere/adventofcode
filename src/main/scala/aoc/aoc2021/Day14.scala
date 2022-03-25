package aoc.aoc2021

import aoc.*

import scala.annotation.tailrec

object Day14 {

  val input = readFile1("aoc2021/Day14.txt")
  val template = input.next()
  val rules = input.drop(1).map{
    case re"(\w\w)$k -> (\w)$v" => k -> v.charAt(0)
  }.toMap

  def performInsert(str: String): String = {
    @tailrec
    def helper(si: List[Char], so: List[Char]): String = {
      si match {
        case List(a, b, _*) => helper(si.tail, so :+ a :+ rules(s"$a$b"))
        case List(a) => (so :+ a).mkString
      }
    }
    helper(str.toList, List[Char]())
  }

  def minMax(vs: Iterable[(_, Int)]): ((_, Int), (_, Int)) = {
    vs.foldLeft((vs.head, vs.head)){case ((mn, mx), v) => (
      if (mn._2 < v._2) mn else v, if (mx._2 > v._2) mx else v)}
  }

  def part1 = {
    val freq = iterateTimes(template, performInsert, 10).groupBy(x => x).map{case (k, v) => k -> v.size}
    val (mn, mx) = minMax(freq)
    mx._2 - mn._2 //2549
  }

  def part2 = {
    val freq = iterateTimes(template, performInsert, 40).groupBy(x => x).map{case (k, v) => k -> v.size}
    val (mn, mx) = minMax(freq)
    mx._2 - mn._2
  }
}
