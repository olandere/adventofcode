package aoc.aoc2016

import aoc.aoc2016.Day1._

class Day1Test extends org.scalatest.funsuite.AnyFunSuite {
  test("move") {
    assert(Day1.move((North, (0, 0), Seq("R2"))) == (East, (2, 0), Seq()))
    assert(Day1.move((East, (2, 0), Seq("L3"))) == (North, (2, 3), Seq()))

    assert(Day1.move((North, (0, 0), Seq("R2"))) == (East, (2, 0), Seq()))
    assert(Day1.move((East, (2, 0), Seq("R2"))) == (South, (2, -2), Seq()))
    assert(Day1.move((South, (2, -2), Seq("R2"))) == (West, (0, -2), Seq()))

    assert(Day1.move((North, (0, 0), Seq("R5"))) == (East, (5, 0), Seq()))
    assert(Day1.move((East, (5, 0), Seq("L5"))) == (North, (5, 5), Seq()))
    assert(Day1.move((North, (5, 5), Seq("R5"))) == (East, (10, 5), Seq()))
    assert(Day1.move((East, (10, 5), Seq("R3"))) == (South, (10, 2), Seq()))
    assert(Day1.move((South, (10, 2), Seq("R3"))) == (West, (7, 2), Seq()))
    assert(Day1.move((West, (7, 2), Seq("R1"))) == (North, (7, 3), Seq()))
    assert(Day1.move((West, (7, 2), Seq("L1"))) == (South, (7, 1), Seq()))

    assert(Day1.move((North, (0, 0), Seq("R8"))) == (East, (8, 0), Seq()))
    assert(Day1.move((East, (8, 0), Seq("R4"))) == (South, (8, -4), Seq()))
    assert(Day1.move((South, (8, -4), Seq("R4"))) == (West, (4, -4), Seq()))
    assert(Day1.move((West, (4, -4), Seq("R8"))) == (North, (4, 4), Seq()))
  }

  test("segment") {
    val s1 = Segment((8,0), (8,-4))
    val s2 = Segment((0,0), (8, 0))
    assert (s2.slope.contains(0))
    assert (s1.slope.isEmpty)

    val s3 = Segment((4,-4), (4, 4))
    assert (s2.intersect(s3) == Some(4, 0))
  }

  test("Day1 part1") {
    assert(Day1.part1 == 250)
  }

  test("Day1 part2") {
    assert(Day1.part2 == 151)
  }

}
