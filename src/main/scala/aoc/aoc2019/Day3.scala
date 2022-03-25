package aoc.aoc2019

import aoc.*

import scala.annotation.tailrec

object Day3 {

  type Segment = ((Int, Int), (Int, Int))

  val input = readFile1("aoc2019/Day3.txt")
  val wire1 = input.next().split(',').scanLeft((0, 0))(toPoint)
  val wire2 = input.next().split(',').scanLeft((0, 0))(toPoint)

  def toPoint(p: (Int, Int), dir: String): (Int, Int) = {
    dir match {
      case re"R(\d+)$d" => (p._1 + d.toInt, p._2)
      case re"L(\d+)$d" => (p._1 - d.toInt, p._2)
      case re"U(\d+)$d" => (p._1, p._2 + d.toInt)
      case re"D(\d+)$d" => (p._1, p._2 - d.toInt)
    }
  }

  def toSegment(s: Segment): aoc.Segment = {
    aoc.Segment(s._1._1, s._1._2, s._2._1, s._2._2)
  }

  def findIntersections(s1: Array[aoc.Segment], s2: Array[aoc.Segment]): Array[(Int, Int)] = {
    (for {
      i <- s1
      j <- s2
    } yield i.intersect(j)).filterNot(_.isEmpty).flatten
  }

  val w1Segments = wire1.zip(wire1.tail).map(toSegment)
  val w2Segments = wire2.zip(wire2.tail).map(toSegment)
  val intersections = findIntersections(w1Segments, w2Segments)

  def pathLength(point: (Int, Int), segs: Array[aoc.Segment]): Int = {
    @tailrec
    def helper(p: (Int, Int), ss: Array[aoc.Segment], len: Int): Int = {
      if (ss.head.contains(p)) {
        len + manhattenDistance(ss.head.x1, ss.head.y1)(p._1, p._2)
      } else helper(p, ss.tail, len + ss.head.length)
    }

    helper(point, segs, 0)
  }

  def pathLengths(points: Array[(Int, Int)], segs: Array[aoc.Segment]) =
    points.map { p => pathLength(p, segs) }

  def part1: Int = {
    intersections.map(manhattenDistance(0, 0)).tail.min // 217
  }

  def part2 = {
    pathLengths(intersections, w1Segments).zip(pathLengths(intersections, w2Segments)).map(p => p._1 + p._2).tail.min // 3454
  }
}
