package aoc.aoc2020

object Day2 {
  import aoc._

  case class Policy(min: Int, max: Int, ch: Char) {
    def isValid(password: String): Boolean = {
      (min to max).contains(password.count(_ == ch))
    }

    def isValid2(password: String): Boolean = {
      password(min-1) == ch ^ password(max-1) == ch
    }
  }

  case object Policy {
    def apply(str: String): Policy = {
      str match {
        case re"(\d+)$mn-(\d+)$mx (\w)$ch" =>
          Policy(mn.toInt, mx.toInt, ch.head)
      }
    }
  }

  case class Entry(policy: Policy, password: String) {
    def isValid: Boolean = {
      policy.isValid(password)
    }

    def isValid2: Boolean = {
      policy.isValid2(password)
    }
  }

  case object Entry {
    def apply(str: String): Entry = {
      val parts = str.split(": ")
      Entry(Policy.apply(parts(0)), parts(1))
    }
  }

  def part1: Int = {
    readFile("aoc2020/Day2.txt").map(Entry.apply).count(_.isValid)
  }

  def part2: Int = {
    readFile("aoc2020/Day2.txt").map(Entry.apply).count(_.isValid2)
  }
}
