package aoc.aoc2015

import scala.collection.mutable

object Day7 {

  import aoc._

  val instructions: Seq[Gate] = processInstructions(readFile("aoc2015/Day7.txt")).toSeq

  sealed trait Gate {
    def process(wires: mutable.Map[String, Int]): Option[Gate]
  }

  case class Not(in: String, out: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] = {
      if (wires.contains(in)) {
        wires(out) = ~wires(in) & 0x0ffff
        None
      } else Some(this)
    }
  }

  case class And(w1: String, w2: String, out: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] = {
      if (wires.contains(w1) && wires.contains(w2)) {
        wires(out) = wires(w1) & wires(w2)
        None
      } else Some(this)
    }
  }

  case class Mask(v: Int, w1: String, out: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] = {
      if (wires.contains(w1)) {
        wires(out) = wires(w1) & v
        None
      } else Some(this)
    }
  }

  case class Or(w1: String, w2: String, out: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] =
      if (wires.contains(w1) && wires.contains(w2)) {
        wires(out) = wires(w1) | wires(w2)
        None
      } else Some(this)
  }

  case class Assign(v: Int, w: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] = {
      wires(w) = v
      None
    }
  }

  case class Copy(w: String, out: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] = {
      if (wires.contains(w)) {
        wires(out) = wires(w)
        None
      } else Some(this)
    }
  }

  case class LShift(w1: String, v: Int, out: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] =
      if (wires.contains(w1)) {
        wires(out) = (wires(w1) << v) & 0x0ffff
        None
      } else Some(this)
  }

  case class RShift(w1: String, v: Int, out: String) extends Gate {
    override def process(wires: mutable.Map[String, Int]): Option[Gate] =
      if (wires.contains(w1)) {
        wires(out) = wires(w1) >>> v
        None
      } else Some(this)
  }

  def processInstructions(instructions: Iterator[String]): Iterator[Gate] = {
    instructions.map {
      case re"(\d+)$v -> ([a-z]+)$w" => Assign(v.toInt, w)
      case re"([a-z]+)$w1 -> ([a-z]+)$w2" => Copy(w1, w2)
      case re"([a-z]+)$w1 AND ([a-z]+)$w2 -> ([a-z]+)$w3" => And(w1, w2, w3)
      case re"(\d+)$v AND ([a-z]+)$w1 -> ([a-z]+)$w2" => Mask(v.toInt, w1, w2)
      case re"([a-z]+)$w1 OR ([a-z]+)$w2 -> ([a-z]+)$w3" => Or(w1, w2, w3)
      case re"([a-z]+)$w1 LSHIFT (\d+)$v -> ([a-z]+)$w2" => LShift(w1, v.toInt, w2)
      case re"([a-z]+)$w1 RSHIFT (\d+)$v -> ([a-z]+)$w2" => RShift(w1, v.toInt, w2)
      case re"NOT ([a-z]+)$w1 -> ([a-z]+)$w2" => Not(w1, w2)
    }
  }

  def part1: Int = {
    val wires: mutable.Map[String, Int] = mutable.HashMap()
    iterate[Seq[Gate]](instructions, i => i.flatMap(_.process(wires)), i => i.isEmpty)
    wires("a") //46065
  }

  def part2: Int = {
    def replace(is: Seq[Gate], rp: Int): Seq[Gate] = {
      is.map {
        case Assign(_, "b") => Assign(rp, "b")
        case x: Gate => x
      }
    }

    val wires: mutable.Map[String, Int] = mutable.HashMap()
    iterate[Seq[Gate]](replace(instructions, part1), i => i.flatMap(_.process(wires)), i => i.isEmpty)
    wires("a") //14134
  }
}
