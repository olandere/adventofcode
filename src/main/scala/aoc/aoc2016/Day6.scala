package aoc.aoc2016

object Day6 {
  
  import aoc._
  
  val lines = readFile1("aoc2016/Day6.txt").toSeq
  
  def mostFrequest(ls: Seq[String], pos: Int) = {
    ls.map(_(pos)).groupBy(x => x).map{case(k, v) => k -> v.length}.toSeq.sortBy(p => -p._2).head._1
  }

  def leastFrequest(ls: Seq[String], pos: Int) = {
    ls.map(_(pos)).groupBy(x => x).map{case(k, v) => k -> v.length}.toSeq.sortBy(p => p._2).head._1
  }
  
  def part1 = {
    (for {
      i <- 0 until lines.head.length
    } yield mostFrequest(lines, i)).mkString //agmwzecr
  }

  def part2 = {
    (for {
      i <- 0 until lines.head.length
    } yield leastFrequest(lines, i)).mkString //owlaxqvq
  }

}
