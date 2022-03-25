package aoc.aoc2016

object Day7 {

  import aoc._

  val addrs = readFile1("aoc2016/Day7.txt").toSeq

  def isABBA(str: String): Boolean = str(0) == str(3) && str(1) == str(2) && str(0) != str(1)

  def isABA(str: String): Boolean = str(0) == str(2) && str(0) != str(1)

  def toBAB(aba: String): String = aba(1).toString + aba(0) + aba(1)

  def parse(str: String): (Seq[String], Seq[String]) = {
    def helper(str: String, abba: Seq[String], nabba: Seq[String]): (Seq[String], Seq[String]) = {
      if (str.isEmpty) (abba, nabba) else {
        if (str.startsWith("[")) {
          val rb = str.indexOf(']')
          helper(str.substring(rb + 1), abba, nabba :+ str.substring(1, rb))
        } else {
          val lb = str.indexOf('[')
          if (lb > -1)
            helper(str.substring(lb), abba :+ str.substring(0, lb), nabba)
          else
            helper("", abba :+ str, nabba)
        }
      }
    }

    helper(str, Seq(), Seq())
  }

  def supportsTLS(str: String): Boolean = {
    val (abba, nabba) = parse(str)
    if (nabba.exists(_.sliding(4).exists(isABBA))) false
    else abba.exists(_.sliding(4).exists(isABBA))
  }

  def supportsSSL(str: String) = {
    val (aba, bab) = parse(str)
    val as = aba.flatMap(_.sliding(3)).filter(isABA).toSet
    val bs = bab.flatMap(_.sliding(3)).filter(isABA).toSet
    as.exists(s => bs(toBAB(s)))
  }

  def part1 = addrs.count(supportsTLS) //118

  def part2 = addrs.count(supportsSSL) //260
}
