package aoc.aoc2020

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object Day16 {
  import aoc._

  val data: Seq[String] = readFile("aoc2020/Day16.txt").toSeq

  type Ticket = Seq[Int]

  def buildFunc(v1: Int, v2: Int, v3: Int, v4: Int): Int => Boolean = { v =>
    (v1 to v2).contains(v) || (v3 to v4).contains(v)
  }

  def join(f1: Int => Boolean, f2: Int => Boolean): Int => Boolean = {
    v => f1(v) || f2(v)
  }

  def buildRules(data: Iterator[String]): Map[String, Int => Boolean] = {
    val ruleText = data.takeWhile(_.nonEmpty)
    ruleText.map{
      case re"([a-z ]+)$name: (\d+)$v1-(\d+)$v2 or (\d+)$v3-(\d+)$v4" =>
        //println(s"$name: $v1, $v2, $v3, $v4")
        name -> buildFunc(v1.toInt, v2.toInt, v3.toInt, v4.toInt)
    }.toMap
  }

  def toTicket(s: String): Ticket = {
    s.split(",").map(_.toInt)
  }

  def myTicket(data: Iterator[String]): Ticket = {
    data.slice(1, 2).map(toTicket).next()
  }

  def nearByTickets(data: Iterator[String]): Seq[Ticket] = {
    data.drop(2).map(toTicket).toSeq
  }

  def part1: Int = {
    val dataIter = data.iterator
    val isValid = buildRules(dataIter).values.reduce(join)
    val mine = myTicket(dataIter)
    nearByTickets(dataIter).flatMap(_.filterNot(isValid)).sum //27802
  }

  def part2: Long = {

    type Fields = Seq[mutable.HashSet[String]]
    def eliminateFields(ticket: Ticket, rules: Map[String, Int => Boolean]) = {
      for {
        (t, i) <- ticket.zipWithIndex
        (k, v) <- rules if !v(t)
      } yield (k, i)
    }

    val dataIter = data.iterator
    val rules = buildRules(dataIter)
    val isValid = rules.values.reduce(join)
    val isValidTicket: Ticket => Boolean = t => t.forall(isValid)
    val mine = myTicket(dataIter)
    val validTickets = nearByTickets(dataIter).filter(isValidTicket)
    val fieldArray = ArraySeq.fill(20)(mutable.HashSet(rules.keySet).flatten)
    validTickets.foreach{t =>
      eliminateFields(t, rules).foreach { case (f, i) =>
        fieldArray(i).remove(f)
      }
    }

    def eliminateSingletons(fs: (Seq[mutable.Set[String]], Set[String])): (Seq[mutable.Set[String]], Set[String]) = {
      val v = fs._1.find(p => p.size == 1 && !fs._2(p.head))
      v.foreach{ss =>
        fs._1.filterNot{_ == ss}.foreach{_.remove(ss.head)}
      }
      (fs._1, v.fold(fs._2)(e => fs._2 ++ e))
    }

    def allSingleton(fs: (Seq[mutable.Set[String]], _)): Boolean = {
      fs._1.forall(_.size == 1)
    }

    iterate((fieldArray, Set[String]()), eliminateSingletons, allSingleton)
    fieldArray.zip(mine).filter(_._1.head.startsWith("departure")).map(_._2.toLong).product //279139880759
  }
}
