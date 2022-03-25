package aoc.aoc2020

import scala.collection.mutable

object Day14 {
  import aoc._

  val program: Seq[String] = readFile("aoc2020/Day14.txt").toSeq
  var mask: String = ""

  def toBinary(v: String): String = {
    val s = v.toInt.toBinaryString
    "0".repeat(36-s.length)+s
  }

  def sumMemory(mem: mutable.Map[_, BigInt]): BigInt = {
    mem.values.sum
  }

  def run(setMem: (String, String, String) => Unit): Unit = {
    program.foreach {
      case re"mask = ([01X]+)$m" => mask = m
      case re"mem\[([0-9]+)$addr\] = ([0-9]+)$v" => setMem(addr, v, mask)
    }
  }

  def part1: BigInt = {

    val mem: mutable.Map[String, BigInt] = mutable.HashMap()

    def setMem(addr: String, v: String, m: String): Unit = {
      //println(s"[$addr] = $v($m)")
      val newV = toBinary(v).zip(m).map{
        case (c, 'X') => c
        case (_, v) => v
      }.mkString
      //println(s"Masked Value: $newV")
      mem(addr) = BigInt(newV, 2)
    }

    run(setMem)
    sumMemory(mem) //10050490168421
  }

  def part2: BigInt = {
    val mem: mutable.Map[String, BigInt] = mutable.HashMap()

    def processFloatingBits(addr: String): Seq[String] = {
      def replace(addr: String): Seq[String] = {
        val pos = addr.indexOf('X')
        if (pos < 0) Seq()
        else {
          Seq(addr.substring(0, pos) + "0" + addr.substring(pos+1),
            addr.substring(0, pos) + "1" + addr.substring(pos+1))
        }
      }
      replace(addr)
    }

    def setMem(addr: String, v: String, m: String): Unit = {
      //val value = v.toInt
      val address = toBinary(addr).zip(m).map{
        case (_, 'X') => 'X'
        case (c, '0') => c
        case (_, '1') => '1'
      }.mkString
     // val addresses = processFloatingBits(address)

      val na = iterate[Seq[String]](Seq(address), as => as.flatMap(processFloatingBits), as => !as.head.contains("X"))
      //val na = addresses.flatMap(processFloatingBits)
      na.foreach(a => mem(a) = BigInt(v))
     // println(na)
    }

    run(setMem)
    sumMemory(mem)
  }
}
