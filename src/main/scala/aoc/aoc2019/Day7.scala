package aoc.aoc2019

import aoc.readFile1

object Day7 {

  val intcode = readFile1("aoc2019/Day7.txt").next().split(',').map(_.toInt).toSeq

  def allPhases(s: Set[Int]) = {
    for {
      a <- s
      b <- s - a
      c <- s.diff(Set(a, b))
      d <- s.diff(Set(a, b, c))
      e <- s.diff(Set(a, b, c, d))
    } yield Seq(a, b, c, d, e)
  }

  def runProgram(code: Array[Int], phase: Int, inSignal: Int) = {
    val ic = Intcode()
    ic.input.enqueue(phase)
    ic.input.enqueue(inSignal)
    ic.run(code, 0)
    ic.output
  }

  def runAll =
  //    allPhases(Set(0, 1, 2, 3, 4)).map {ps =>
    Seq(0, 1, 2, 3, 4).permutations.map(
      _.foldLeft(0)((o, p) => runProgram(intcode.toArray, p, o))
    )


  def part1 = {
    runAll.max // 46248
  }

  def part2 = {

  }
}
