package aoc.aoc2021

import aoc.*

import scala.annotation.tailrec

object Day9 {
  val input: Array[Array[Int]] = readFile1("aoc2021/Day9.txt").toArray.map(_.toArray.map(_.asDigit))

  def getHeight(x: Int, y: Int): Int = input(x)(y)

  val boundedNeighbors = neighbors(input.indices, input(0).indices)

  def myNeighbors(i: Int, j: Int) = boundedNeighbors(i, j).filter(filterDiagonals(i, j))

  def getNeighborHeights(i: Int, j: Int) = {
    myNeighbors(i, j).map(getHeight.tupled)
  }

  def getNeighborsInBasin(i: Int, j: Int): Set[(Int, Int)] = {
    myNeighbors(i, j).filterNot(p => getHeight.tupled(p) == 9)
  }

  def part1 = {
    val lows = for {
      i <- input.indices
      j <- input(0).indices if (getNeighborHeights(i,j).forall(_ > input(i)(j)))
    } yield input(i)(j)
    lows.size + lows.sum // 528
  }
  
  def findBasins(low: Seq[(Int, Int)]) = {
    @tailrec
    def findBasin(frontier: Set[(Int, Int)], basin: Set[(Int, Int)] ): Set[(Int, Int)] = {
//      println(s"frontier: $frontier")
//      println(s"basin: $basin")
      if (frontier.isEmpty) basin else {
        val newBasin = basin ++ frontier
        findBasin(frontier.flatMap(getNeighborsInBasin.tupled).diff(newBasin), newBasin)
      }
    }
    low.map{x => findBasin(Set(x), Set())}
  }
  
  def part2 = {
    val lowCoords = for {
      i <- input.indices
      j <- input(0).indices if (getNeighborHeights(i,j).forall(_ > input(i)(j)))
    } yield (i, j)

    findBasins(lowCoords).map(_.size).sortBy(x => -x).take(3).product // 920448
  }
}
