package aoc.aoc2021

import aoc.aoc2021.Day15.*

class Day15Test extends org.scalatest.funsuite.AnyFunSuite {

  test("test1") {
    assert(merge(Seq[Int](), Seq()) == Seq())
    assert(merge(Seq("1"), Seq()) == Seq("1"))
    assert(merge(Seq(), Seq("1")) == Seq("1"))
    assert(merge(Seq("1"), Seq("2")) == Seq("1", "2"))
    assert(merge(Seq("2"), Seq("1")) == Seq("1", "2"))
    assert(merge(Seq("1", "2"), Seq("3")) == Seq("1", "2", "3"))
    assert(merge(Seq("2"), Seq("1", "3")) == Seq("1", "2", "3"))
    assert(merge(Seq("1"), Seq("2", "3")) == Seq("1", "2", "3"))
    assert(merge(Seq("2", "3"), Seq("1")) == Seq("1", "2", "3"))
    assert(merge(Seq("1", "3"), Seq("2")) == Seq("1", "2", "3"))
    assert(merge(Seq("1", "3"), Seq("2", "4")) == Seq("1", "2", "3", "4"))
    assert(merge(Seq(2, 4), Seq(1, 3)) == Seq(1, 2, 3, 4))
  }

  test("test2") {
    val p1 = Path(1, 0, 1)
    val p2 = Path(0, 1, 2)
    val p3 = Path(2, 1, 7)
    val p4 = Path(1, 1, 2)

    assert(merge(Seq(p1), Seq(p2)) == Seq(p1, p2))
    assert(merge(Seq(p1, p3), Seq(p2)) == Seq(p1, p2, p3))
    assert(merge(Seq(p1, p3), Seq(p4, p2)) == Seq(p1, p4, p2, p3))
  }

  test("test3") {
    val p1 = Path(1, 2, 2)
    val p2 = Path(2, 1, 1)
    val p3 = Path(3, 1, 7)
    val p4 = Path(4, 1, 8)
    val p5 = Path(1, 2, 1)

    assert(merge(Seq(p1), Seq(p5)) == Seq(p5))
  }
  
  test("verify reverse") {
    
  }

}
