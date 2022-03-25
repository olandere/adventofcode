package aoc.aoc2020

class Day13Test extends org.scalatest.funsuite.AnyFunSuite {
  test("Day13 part1")  {
    assert(Day13.part1 == 2165)
  }

  test("Day13 part2")  {
    assert(Day13.part2 == (BigInt("534035653563227"), BigInt("1797379356693401")))
  }

}
