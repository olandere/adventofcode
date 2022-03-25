package aoc.aoc2020

import scala.util.Try

object Day4 {
  import aoc._

  val passports: Seq[String] = readFile("aoc2020/Day4.txt").toSeq

  case class Passport(data: Map[String, String]) {
    val fields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")

    def isPresent: Boolean = {
      def missingFields: Set[String] = fields &~ data.keySet

      missingFields.isEmpty || (missingFields("cid") && missingFields.size == 1)
    }

    def toInt(s: String): Int = Try(s.toInt).getOrElse(-1)

    def isInRange(f: String, low: Int, high: Int): Boolean = {
      low to high contains toInt(f)
    }

    val colors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

    def isValid: Boolean =
      fields.forall(field => {
      field match {
        case "byr" => isInRange(data(field), 1920, 2002)
        case "iyr" => isInRange(data(field), 2010, 2020)
        case "eyr" => isInRange(data(field), 2020, 2030)
        case "hgt" => data(field) match {
          case re"(\d+)cm$v" => isInRange(v, 150, 193)
          case re"(\d+)in$v" => isInRange(v, 59, 76)
          case _ => false
        }
        case "hcl" => """#[a-f0-9]{6}""".r.matches(data(field))
        case "ecl" => colors(data(field))
        case "pid" => "(\\d){9}".r.matches(data(field))
        case "cid" => true
      }
    })
  }

  class PassportIterator(iter: Iterator[String]) extends Iterator[Passport] {
    override def hasNext: Boolean = iter.hasNext

    override def next(): Passport = {
      val rec: Map[String, String] = iter.takeWhile(_.nonEmpty).mkString(" ").split(" ").map {
        case re"(\S+)$k:(\S+)$v" => k -> v
       }.toMap
      Passport(rec)
    }
  }

  def part1: Int = {
    new PassportIterator(passports.iterator).count(_.isPresent)
  }

  def part2: Int = {
    new PassportIterator(passports.iterator).filter(_.isPresent).count(_.isValid)
  }
}
