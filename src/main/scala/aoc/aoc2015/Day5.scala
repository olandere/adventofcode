package aoc.aoc2015

import scala.annotation.tailrec

object Day5 {

  import aoc._

  val strings: Seq[String] = readFile("aoc2015/Day5.txt").toSeq
  val vowels: Set[Char] = "aeiou".toSet
  def atLeastThreeVowels(str: String): Boolean = {
    str.count(vowels) >= 3
  }

  def hasDoubleLetter(str: String): Boolean = {
    str.zip(str.tail).exists{p => p._1 == p._2}
  }

  val disallowed = Set("ab", "cd", "pq", "xy")

  def isAllowed(str: String): Boolean = {
    !str.sliding(2).exists(disallowed)
  }

  def and[T](f: T =>Boolean, g: T =>Boolean)(t: T): Boolean =
    f(t) && g(t)

  def isNice(str: String): Boolean = {
    and(and(atLeastThreeVowels, hasDoubleLetter), isAllowed)(str)
   // atLeastThreeVowels(str) && hasDoubleLetter(str) && isAllowed(str)
  }

  def part1: Int = {
    strings.count(isNice)
  }

  def part2: Int = {

    def hasNonOverlappingPair(str: String): Boolean = {
      def isOverlap(xs: Seq[(String, Int)]) =
        xs.length == 2 && xs.map{ case (_, i) => i}.reduce{(a, b) => math.abs(a-b)} == 1

      str.sliding(2).zipWithIndex.toSeq.groupBy(_._1).
        filterNot { case (_, v) => v.length == 1 }.exists { case (_, v) => !isOverlap(v) }
    }

    def hasTwoWithOneBetween(str: String): Boolean = {
      @tailrec
      def helper(as: Seq[Char]): Boolean = {
        if (as.length < 3) false
        else as match {
          case Seq(x, _, y, _*) if x == y => true
          case _ => helper(as.tail)
        }
      }
      helper(str.toSeq)
    }

    def isNice(str: String): Boolean = hasTwoWithOneBetween(str)

    strings.count(and(isNice, hasNonOverlappingPair))
  }
}
