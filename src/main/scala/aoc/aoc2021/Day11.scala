package aoc.aoc2021

import aoc.*

import scala.annotation.tailrec

object Day11 {
  val input: Array[Array[Int]] = readFile1("aoc2021/Day11.txt").toArray.map(_.toArray.map(_.asDigit))

  val boundedNeighbors = neighbors(input.indices, input(0).indices)

  var stepCount: Int = 0


  def incrementAll(arr: Array[Array[Int]]) = {
    arr.map { x => x.map(_ + 1) }
  }

  def flashers(arr: Array[Array[Int]]): Set[(Int, Int)] = {
    (for {
      i <- arr.indices
      j <- arr(0).indices if arr(i)(j) > 9
    } yield (i, j)).toSet
  }

  def step(arr: Array[Array[Int]], flashCount: Int) = {
    @tailrec
    def handleFlashes(arr: Array[Array[Int]], flashed: Set[(Int, Int)], cnt: Int): (Array[Array[Int]], Int) = {
      val f = flashers(arr).diff(flashed)
      if (f.isEmpty) (arr, cnt + flashed.size) else {
        val affected = f.toSeq.flatMap(boundedNeighbors.tupled)
        // need to increment each entry in newArr from f
        affected.foreach((i, j) => arr(i)(j) += 1)
        handleFlashes(arr, flashed ++ f, cnt)
      }
    }

    stepCount += 1
    val newArr = incrementAll(arr)
    val (_, cnt) = handleFlashes(newArr, Set(), flashCount)

    (newArr.map{a => a.mapInPlace(x => if (x > 9) 0 else x)}, cnt)
  }

  def printArr(arr: Array[Array[Int]], flashCount: Int) = {
    arr.foreach(a => println(a.mkString))
    println()
    (arr, flashCount)
  }

  def part1 = {
   // val show = printArr.tupled.andThen(step.tupled)
    val (_, count) = iterateTimes((input, 0), step.tupled, 100)
    count // 1741
  }

  def part2 = {
    iterate((input, 0), step.tupled, (a: Array[Array[Int]], _) => a.forall(_.forall(_ == 0)))
    stepCount
  }
}
