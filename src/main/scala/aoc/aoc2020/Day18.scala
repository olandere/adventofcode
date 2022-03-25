package aoc.aoc2020

import scala.annotation.tailrec

object Day18 {
  import aoc._

  val problems: Seq[String] = readFile("aoc2020/Day18.txt").toSeq

  def findSubExpression(str: String): (Int, Int) = {
    @tailrec
    def helper(str: String, cnt: Int, llp: Int): (Int, Int) = {
      if (str.head == '(') helper(str.tail, cnt + 1, cnt)
      else if (str.head == ')') (llp, cnt)
      else helper(str.tail, cnt + 1, llp)
    }

    helper(str, 0, -1)
  }

  def replaceSubExpression(str: String): String = {
    val (llp, frp) = findSubExpression(str)
    val r = eval(str.substring(llp + 1, frp))
    str.substring(0, llp) + r + str.substring(frp + 1)
  }

  def hasNoSubExpression(str: String): Boolean = !str.exists(_ == '(')

  //@tailrec
  def eval1(s: String): Long = {
    // println(s)
    s match {
      case re"(\d+)$x \+ (\d+)$y(.*)$r" => eval(s"${eval(x) + eval(y)}$r") //eval(s"${x.toInt + y.toInt}$r")
      case re"(\d+)$x \* (\d+)$y(.*)$r" => eval(s"${eval(x) * eval(y)}$r") //eval(s"${x.toInt * y.toInt}$r")
      //   case re"(\d+)$x \+ \((.*)$r" => eval(s"${eval(x)} + ( ${eval(r.trim)}")//eval(s"${x.toInt * y.toInt}$r")
      //   case re"\((\d+)$x\)" => x.toInt
      case re"(\d+)$x" => x.toLong
      // case x => x
    }
  }

  def eval(s: String): Long = {
    // println(s)
    s match {
      case re"(\d+)$x \+ (\d+)$y(.*)$r" => eval(s"${eval(x) + eval(y)}$r") //eval(s"${x.toInt + y.toInt}$r")
      case re"(\d+)$x \* (.*)$r" => eval(s"${eval(x) * eval(r)}") //eval(s"${x.toInt * y.toInt}$r")
      //   case re"(\d+)$x \+ \((.*)$r" => eval(s"${eval(x)} + ( ${eval(r.trim)}")//eval(s"${x.toInt * y.toInt}$r")
      //   case re"\((\d+)$x\)" => x.toInt
      case re"(\d+)$x" => x.toLong
      // case x => x
    }
  }

  def part1: Long = {
    problems.map { p => eval(iterate(p, replaceSubExpression, hasNoSubExpression)) }.sum //16332191652452
  }

  def part2: Long = {
    problems.map { p => eval(iterate(p, replaceSubExpression, hasNoSubExpression)) }.sum //351175492232654
  }
}
