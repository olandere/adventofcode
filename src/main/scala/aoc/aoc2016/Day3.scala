package aoc.aoc2016

import scala.annotation.tailrec

object Day3 {
  import aoc._

  val triangles: Seq[Array[Int]] = readFile1("aoc2016/Day3.txt").map(_.trim.split("\\s+").map{_.toInt}).toSeq

  def isTriangle(arr: Array[Int]): Boolean = {
    val Array(a, b, c) = arr
    a + b > c && a + c > b && b + c > a
  }

  def transpose(arr: Seq[Array[Int]]): Seq[Array[Int]] = {
    val Array(a,b,c) = arr(0)
    val Array(d,e,f) = arr(1)
    val Array(g,h,i) = arr(2)

    Seq(
      Array(a,d,g),
      Array(b,e,h),
      Array(c,f,i)
    )
  }

  def doTranspose(arr: Seq[Array[Int]]): Seq[Array[Int]] = {
    if (arr.isEmpty) arr else
    transpose(arr.take(3)) ++ doTranspose(arr.drop(3))
  }

  def part1: Int = {
    triangles.count(isTriangle) //983
  }

  def part2: Int = {
    doTranspose(triangles).count(isTriangle) //1836
  }
}
