package aoc.aoc2020

import scala.io.Source

object Day6 {

  import aoc._
  val answers: Seq[String] = readFile("aoc2020/Day6.txt").toSeq

  class CharSetIterator(iter: Iterator[String]) extends Iterator[Set[Char]] {
    def addSets(s: Set[Char], e: String): Set[Char] = s ++ e.toSet

    override def hasNext: Boolean = iter.hasNext

    override def next(): Set[Char] = iter.takeWhile(_.nonEmpty).foldLeft(Set[Char]())(addSets)
  }

  class CharSetIntersectIterator(iter: Iterator[String]) extends Iterator[Set[Char]] {
    def intersectSets(s: Set[Char], e: String): Set[Char] = s.intersect(e.toSet)

    override def hasNext: Boolean = iter.hasNext

    override def next(): Set[Char] = iter.takeWhile(_.nonEmpty).foldLeft("abcdefghijklmnopqrstuvwxyz".toSet)(intersectSets)
  }

  def part1: Int = {
    new CharSetIterator(answers.iterator).map(_.size).sum
  }

  def part2: Int = {
    new CharSetIntersectIterator(answers.iterator).map(_.size).sum
  }
}