package aoc.aoc2016

import scala.collection.SortedSet

object Day4 {

  import aoc._

  val rooms: Seq[Entry] = readFile1("aoc2016/Day4.txt").map(Entry.apply).toSeq

  case class Entry(name: String, sid: Int, cs: String) {

    def isValid: Boolean = {
      val ss = SortedSet.from(name.replace("-", "").
        groupBy { x => x }.map { case (k, v) => k -> v.length })(Ordering[(Int, Char)].on { x => (-x._2, x._1) })
      cs == ss.toSeq.take(5).map(_._1).mkString
    }
    
    def decrypt: String = {
      val rot = sid % 26
      name.map{
        case '-' => ' '
        case x => 
          val ch = x + rot
          if (ch > 122) (ch - 26).toChar else ch.toChar
      }
    }
  }

  case object Entry {
    def apply(str: String): Entry = str match {
      case re"([a-z\-]+)$name-([\d]+)$sid\[([a-z]{5})$cs\]" =>
        new Entry(name, sid.toInt, cs)
    }
  }

  def validateChecksum(entry: Entry): Boolean = {
    false
  }

  def part1: Int = {
    rooms.filter(_.isValid).map(_.sid).sum
  }
  
  def part2 = {
    rooms.filter(_.isValid).map(r => (r.decrypt, r.sid)).filter(c => c._1.contains("north"))
  }
}
