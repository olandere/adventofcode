package aoc.aoc2016

import org.scalatest.funsuite.AnyFunSuite
import aoc.aoc2016.Day7._

class Day7Test extends AnyFunSuite {

  test("isABBA") {
    assert(!Day7.isABBA("abcd"))
    assert(!Day7.isABBA("abca"))
    assert(!Day7.isABBA("abbc"))
    assert(!Day7.isABBA("aaaa"))
    assert(Day7.isABBA("xyyx"))
  }
}
