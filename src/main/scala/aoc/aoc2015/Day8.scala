package aoc.aoc2015

import scala.annotation.tailrec

object Day8 {

  import aoc._

  val strings: Seq[String] = readFile("aoc2015/Day8.txt").toSeq

  val isHex = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  def memLength1(str: String): Int = {
    str.substring(1, str.length - 1).split('\\').map {
      case "" => 1
      case re"x[0-9a-f]{2}(.*)$x" => x.length + 1
      case x => x.length
    }.sum
  }

  def memLength(str: String): Int = {
    @tailrec
    def helper(s: Seq[Char], acc: Int): Int = {
      if (s.isEmpty) acc else
        s.head match {
          case '\\' => s.tail match {
            case x@Seq('\\', _*) => helper(x.tail, acc + 1)
            case x@Seq('"', _*) => helper(x.tail, acc + 1)
            case x@Seq('x', d1, d2, _*) if isHex(d1) && isHex(d2) => helper(x.drop(3), acc + 1)
            //case x@_ => helper(x.tail, acc+1)
          }
          case _ => helper(s.tail, acc + 1)
        }
    }

    helper(str.substring(1, str.length - 1).toSeq, 0)
  }

  def encodeLen(str: String): Int = {
    @tailrec
    def helper(s: Seq[Char], acc: Int): Int = {
      if (s.isEmpty) acc else
        s.head match {
          case '\\' => helper(s.tail, acc + 2)
          case '"' => helper(s.tail, acc + 2)
          case _ => helper(s.tail, acc + 1)
        }
    }

    helper(str.substring(1, str.length - 1).toSeq, 6)
  }

  def part1: Int = {
    strings.map(s => s.length - memLength(s)).sum //1350
  }

  def part2: Int = {
    strings.map { s => encodeLen(s) - s.length }.sum //2085
  }
}
