package aoc.aoc2021

import aoc.*

import scala.annotation.tailrec

object Day14 {

  val input: Iterator[String] = readFile1("aoc2021/Day14.txt")
  val template: String = input.next()
  val rules: Map[String, Char] = input.drop(1).map{
    case re"(\w\w)$k -> (\w)$v" => k -> v.charAt(0)
  }.toMap

  def toPairs(str: String): Seq[String] = {
    Seq(s"${str(0)}${rules(str)}", s"${rules(str)}${str(1)}")
  }

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

  def minMax(vs: Iterable[(_, Long)]): ((_, Long), (_, Long)) = {
    vs.foldLeft((vs.head, vs.head)){case ((mn, mx), v) => (
      if (mn._2 < v._2) mn else v, if (mx._2 > v._2) mx else v)}
  }

  def part1old: Long = {
    val freq = iterateTimes(template, performInsert, 10).groupBy(x => x).map{case (k, v) => k -> v.length.toLong}
    val (mn, mx) = minMax(freq)
    mx._2 - mn._2 //2549
  }

  def performInserts(pairs: Map[String, Long]): Seq[(String, Long)] = {
    pairs.toList.flatMap{(k, v) => toPairs(k).map(_ -> v)
    }
  }

  def toMap(l: Seq[(String, Long)]): Map[String, Long] = {
    l.groupBy(x=>x._1).map((k, v) => k -> v.map(_._2).sum)
  }

  def freqMap(m: Map[String, Long]): Map[Char, Long] = {
    val f = m.toSeq.flatMap{(k, v) => Seq(k(0) -> v, k(1) ->v)}.groupBy(_._1).map{(k,v) => k -> v.map(_._2).sum}
    f.map{(k, v) => k -> (if ((v & 1) == 1) v / 2 + 1L else v / 2L)}
  }

  def part1: Long = {
    val b = template.sliding(2).toSeq.groupBy(x=>x).map((k, v) => k -> v.length.toLong)
    val (mn, mx) = minMax(freqMap(iterateTimes(b, performInserts.andThen(toMap), 10)))
    mx._2 - mn._2  //2549
  }

  def part2: Long = {
    val b = template.sliding(2).toSeq.groupBy(x=>x).map((k, v) => k -> v.length.toLong)
    val (mn, mx) = minMax(freqMap(iterateTimes(b, performInserts.andThen(toMap), 40)))
    mx._2 - mn._2
  }
}
