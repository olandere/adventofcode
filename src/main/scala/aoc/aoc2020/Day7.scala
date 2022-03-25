package aoc.aoc2020

import scala.annotation.tailrec

object Day7 {
   import aoc._

   val rules: Seq[String] = readFile("aoc2020/Day7.txt").toSeq

   def parseLine(line: String): Seq[(String, String)] = {
      val l = line.split(" bags contain ").toSeq
      val t = l.tail.head.split(" bag[s]?, ").toSeq.
        map(_.replaceAll(" bag[s]?.", "")).
        map(_.replaceAll("\\d+", "").trim)
      t.map(_ -> l.head)
   }

   val bagToParent: Map[String, Seq[String]] = rules.flatMap(parseLine).groupBy(_._1).map{case (k, v) => k -> v.map(_._2)}.withDefaultValue(Seq.empty)

   case class Bag(name: String, count: Int) {}

   def parseLineToBags(line: String): (String, Seq[Bag]) = {
      val l = line.split(" bags contain ").toSeq
      val t = l.tail.head.split(" bag[s]?, ").toSeq.
        map(_.replaceAll(" bag[s]?.", "")).map{
         case re"(\d+)$n ([a-z ]+)$b" => Bag(b, n.toInt)
         case "no other" => Bag("none", 0)
      }
      l.head -> t
   }

   val bags: Map[String, Seq[Bag]] = rules.map(parseLineToBags).toMap.withDefaultValue(Seq.empty)


   def part1: Int = {

      @tailrec
      def containingBags(acc: Set[String], parents: Set[String]): Set[String] = {
         val notSearched = parents &~ acc
         if (notSearched.isEmpty) acc
         else
            containingBags(acc ++ parents, notSearched.flatMap(bagToParent))
      }
      containingBags(Set[String](), bagToParent("shiny gold").toSet).size
   }

   def part2: Int = {

      def numOfBags(bag: String): Int = {
         //println(bag)
         bags(bag).map(b => b.count + b.count* numOfBags(b.name)).sum
      }
      numOfBags("shiny gold")
   }
}
