package aoc.aoc2016

object Day10 {

  import aoc._
  import scala.collection.mutable

  val instructions: Seq[Instruction] = readFile1("aoc2016/Day10.txt").map(parse).toSeq
  
  trait Sink {
    def set(v: Int): Boolean
  }
  
  object Sinks {
    def apply(id: Int, t: String): Sink = {
      t match {
        case "bot" => Bots(id)
        case "output" => Outputs(id)
      }
    }
  }
  
  object Outputs {
    var outputs: mutable.Map[Int, Output] = mutable.HashMap[Int, Output]()

    def apply(id: Int): Output = outputs.getOrElseUpdate(id, new Output(id))

    def reset = outputs.clear()
  }
  
  class Output(val id: Int) extends Sink {
    val values = mutable.SortedSet[Int]()

    def set(v: Int): Boolean = {
      println(s"Output($id).set($v)")
      values.add(v)
      true
    }

    override def toString: String = s"Output($id): $values"
  }

  object Bots {
    var bots: mutable.Map[Int, Bot] = mutable.HashMap[Int, Bot]()

    def apply(id: Int): Bot = bots.getOrElseUpdate(id, new Bot(id))
    
    def exists(l: Int, h: Int) = {
      bots.values.find(b => b.values.contains(l) && b.values.contains(h))
    }
    
    def reset = bots.clear()
  }

  class Bot(val id: Int) extends Sink {
    val values = mutable.SortedSet[Int]()

    def set(v: Int): Boolean = {values.add(v); true}

    def gives(lb: Sink, hb: Sink): Boolean = {
      println(s"values: $values")
      if (values.size == 2) {
        val Seq(low, high) = values.toSeq
        println(s"in gives: $id, $low, $high")
        lb.set(low) && hb.set(high)
        true
      } else false
    }

    override def toString: String = s"Bot($id): $values"
  }

  sealed trait Instruction {
    def process(): Boolean
  }

  case class Gives(b: Bot, lb: Sink, hb: Sink) extends Instruction {
    def process(): Boolean = {
      b.gives(lb, hb)
    }
  }

  case class SetValue(v: Int, b: Bot) extends Instruction {
    def process(): Boolean = {
      val rc = b.set(v)
      true
    }
  }


  def parse(inst: String): Instruction = {
    inst match {
      //bot or output
      case re"bot (\d+)$b gives low to (bot|output)$t1 (\d+)$lb and high to (bot|output)$t2 (\d+)$hb" => Gives(Bots(b.toInt), Sinks(lb.toInt, t1), Sinks(hb.toInt, t2))
      case re"value (\d+)$v goes to bot (\d+)$b" => SetValue(v.toInt, Bots(b.toInt))
    }
  }

  def part1 = {
    iterate(instructions, i => i.filterNot(_.process()), i => Bots.exists(17, 61).isDefined || i.isEmpty)
    Bots.exists(17, 61) //86
  }
  
  def part2 = {
   // Bots.reset
   // Outputs.reset
    iterate(instructions, i => i.filterNot(_.process()), i => i.isEmpty)
    Outputs(0).values.head * Outputs(1).values.head * Outputs(2).values.head //22847
  }
}
