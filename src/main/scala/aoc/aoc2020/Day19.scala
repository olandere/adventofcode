package aoc.aoc2020

import scala.util.matching.Regex

object Day19 {
  import aoc._

  val input: Iterator[String] = readFile("aoc2020/Day19.txt")

  def populateRules(lines: Iterator[String]): Map[String, String] = {
    lines.takeWhile(!_.isBlank).map{case re"(\d+)$k: (.*)$v" => k -> v.replaceAll("\"", "")}.toMap
  }

//  val rules = Map("0" -> "4 1 5",
//    "1" -> "2 3 | 3 2",
//    "2" -> "4 4 | 5 5",
//    "3" -> "4 5 | 5 4",
//    "4" -> "a",
//    "5" -> "b").withDefault(s => s)

  val rules: Map[String, String] = populateRules(input)
  val strings: Seq[String] = input.toSeq

  sealed trait Atom {
    def rewrite: Seq[Atom]

    def isTerminal = true
  }

  case object Atom {
    def apply(str: String): Atom = {
      if (rules.contains(str)) NonTerminal(str) else {
        if (str == "|") OR else Terminal(str)
      }
    }
  }

  case object LP extends Atom {
    override def toString: String = "("

    override def rewrite: Seq[Atom] = Seq(LP)
  }

  case object RP extends Atom {
    override def toString: String = ")"

    override def rewrite: Seq[Atom] = Seq(RP)
  }

  case object OR extends Atom {
    override def toString: String = "|"

    override def rewrite: Seq[Atom] = Seq(OR)
  }

  case class Terminal(t: String) extends Atom {
    override def toString: String = t

    override def rewrite: Seq[Atom] = Seq(this)
  }

  case class NonTerminal(nt: String) extends Atom {
    override def toString: String = nt

    override
    def isTerminal = false

    override def rewrite: Seq[Atom] = {
      def makeSubExpr(str: String): Seq[Atom] =
        if (str.contains('|') ) {
          Seq(LP) ++ str.split(' ').map(Atom.apply) ++ Seq(RP)
        } else str.split(' ').map(Atom.apply)

      nt.split(' ').map{rules}.flatMap(makeSubExpr)
    }
  }

  def substitute(str: String, rules: Map[String, String]): String = {
    def makeSubExpr(str: String) = if (str.contains('|') && str.length > 1) s" ( $str ) " else str

    str.split(' ').map{rules}.map(makeSubExpr).mkString
  }

  def buildRegex(start: String): String =
    iterate[Seq[Atom]](Seq(Atom(start)), s => s.flatMap(_.rewrite), s => s.forall(_.isTerminal)).mkString

  def part1: Int = {
    val regex = new Regex(buildRegex("0"))
    strings.count(regex.matches) //222
  }

  def part2: Int = {
    println(buildRegex("42"))
    println(buildRegex("31"))
    7
  }
}
