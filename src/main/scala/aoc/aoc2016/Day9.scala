package aoc.aoc2016

import scala.annotation.tailrec

object Day9 {
  import aoc._
  
  val text = readFile1("aoc2016/Day9.txt").next()

  def processMarker(len: Int, times: Int, str: String): (String, String) = {
    (str.take(len).repeat(times), str.drop(len))
  }

  def decompress(str: String): String = {
    @tailrec
    def helper(str: String, acc: String): String = {
      if (str.isEmpty) acc else {
        val (decomp, rest) = str match {
          case re"\((\d+)$l[x](\d+)$t\)(.*)$r" => processMarker(l.toInt, t.toInt, r)
          case _ => (str.head, str.tail)
        }
        helper(rest, acc + decomp)
      }
    }
    helper(str, "")
  }

  def decompressLen(str: String): Long = {
    @tailrec
    def helper(str: String, acc: Long): Long = {
      if (str.isEmpty) acc else {
        val (rest, len) = str match {
          case re"\((\d+)$l[x](\d+)$t\)(.*)$r" => (r.drop(l.toInt), t.toLong * decompressLen(r.take(l.toInt)))
          case _ => (str.tail, 1L)
        }
        helper(rest, acc + len)
      }
    }
    helper(str, 0L)
  }
  
  def part1 = decompress(text).length //152851
  
  def part2 = decompressLen(text) //11797310782
}
