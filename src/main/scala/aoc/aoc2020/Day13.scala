package aoc.aoc2020

object Day13 {
  import aoc._

  val schedule: Seq[String] = readFile("aoc2020/Day13.txt").toSeq

  def closestTime(t: Int, b: Int): Int = t + (b - (t%b))

  def part1: Int = {
    val schIter = schedule.iterator
    val earliestDepart = schIter.next().toInt
    val busses: Array[Int] = schIter.next().split(",").filterNot(_ == "x").map(_.toInt)
    val soonest = busses.map{b => b -> closestTime(earliestDepart, b)}.minBy(_._2)
    soonest._1 * (soonest._2 - earliestDepart) //2165
  }

  def part2: (BigInt, BigInt) = {
    def solveXTuples(t1: (BigInt, BigInt), t2: (BigInt, BigInt)) = {
      //println(s"$t1, $t2")
      solveX(t1._1, t1._2, t2._1, t2._2).get
    }
    val schIter = schedule.iterator
    val entries = schIter.drop(1).next().split(",").zipWithIndex.filterNot(_._1 == "x").map(e => (BigInt(e._1.toInt), BigInt(-e._2)).swap)
    //println(entries.toList)
    entries.reduce{solveXTuples} // (534035653563227,1797379356693401)

  }
}
