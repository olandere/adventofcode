package aoc.aoc2019

import scala.collection.mutable.Queue

class Intcode {

  val input: Queue[Int] = Queue.empty
  var output: Int = 0

  def run(code: Array[Int], pc: Int): Array[Int] = {
    val op = Opcode(code(pc))
    op.run(code, pc)
    if (!op.isHalt) run(code, pc + op.stepSize) else code
  }

  sealed trait Opcode(parameterMode: Int) {
    def stepSize: Int = 4

    val isHalt = false

    def isImmediate(pos: Int) = (parameterMode & pos) >= 1

    def run(memory: Array[Int], pc: Int): Int

    def getParameter(memory: Array[Int], pc: Int, pos: Int) = {
      if (isImmediate(pos)) memory(pc + pos) else memory(memory(pc + pos))
    }
  }

  case class Add(parameterMode: Int) extends Opcode(parameterMode: Int) {

    override def run(memory: Array[Int], pc: Int): Int = {
      memory(memory(pc + 3)) = getParameter(memory, pc, 1) + getParameter(memory, pc, 2)
      1
    }
  }

  case class Multiply(parameterMode: Int) extends Opcode(parameterMode: Int) {

    override def run(memory: Array[Int], pc: Int): Int = {
      memory(memory(pc + 3)) = getParameter(memory, pc, 1) * getParameter(memory, pc, 2)
      1
    }

  }

  case class Save(parameterMode: Int) extends Opcode(parameterMode: Int) {
    override def stepSize: Int = 2

    override def run(memory: Array[Int], pc: Int): Int = {
      val in = if (input.nonEmpty) input.dequeue()
      else scala.io.StdIn.readInt()
      memory(memory(pc + 1)) = in
      1
    }

  }

  case class Output(parameterMode: Int) extends Opcode(parameterMode: Int) {
    override def stepSize: Int = 2

    override def run(memory: Array[Int], pc: Int): Int = {
      val p = getParameter(memory, pc, 1)
      println(p)
      output = p
      p
    }
  }

  case class JumpIfTrue(parameterMode: Int) extends Opcode(parameterMode: Int) {
    override def stepSize: Int = step

    var step = 3

    override def run(memory: Array[Int], pc: Int): Int = {
      step = if (getParameter(memory, pc, 1) != 0) getParameter(memory, pc, 2) - pc else 3
      step
    }
  }

  case class JumpIfFalse(parameterMode: Int) extends Opcode(parameterMode: Int) {
    override def stepSize: Int = step

    var step = 3

    override def run(memory: Array[Int], pc: Int): Int = {
      step = if (getParameter(memory, pc, 1) == 0) getParameter(memory, pc, 2) - pc else 3
      step
    }
  }

  case class LessThan(parameterMode: Int) extends Opcode(parameterMode: Int) {
    override def run(memory: Array[Int], pc: Int): Int = {
      memory(memory(pc + 3)) = if (getParameter(memory, pc, 1) < getParameter(memory, pc, 2)) 1 else 0
      1
    }
  }

  case class Equals(parameterMode: Int) extends Opcode(parameterMode: Int) {
    override def run(memory: Array[Int], pc: Int): Int = {
      memory(memory(pc + 3)) = if (getParameter(memory, pc, 1) == getParameter(memory, pc, 2)) 1 else 0
      1
    }
  }

  case class Halt(parameterMode: Int) extends Opcode(parameterMode: Int) {
    override def stepSize: Int = 1

    override val isHalt: Boolean = true

    override def run(memory: Array[Int], pc: Int): Int = -1
  }

  object Opcode {
    def apply(opcode: Int) = {
      val op = opcode % 100
      val paramMode = Integer.parseInt((opcode / 100).toString, 2)
      op match {
        case 1 => Add(paramMode)
        case 2 => Multiply(paramMode)
        case 3 => Save(paramMode)
        case 4 => Output(paramMode)
        case 5 => JumpIfTrue(paramMode)
        case 6 => JumpIfFalse(paramMode)
        case 7 => LessThan(paramMode)
        case 8 => Equals((paramMode))
        case 99 => Halt(paramMode)
      }
    }
  }

  def test = {
    assert(run(Array(1, 0, 0, 0, 99), 0) sameElements Array(2, 0, 0, 0, 99))
    assert(run(Array(2, 3, 0, 3, 99), 0) sameElements Array(2, 3, 0, 6, 99))
    assert(run(Array(2, 4, 4, 5, 99, 0), 0) sameElements Array(2, 4, 4, 5, 99, 9801))
    assert(run(Array(1, 1, 1, 4, 99, 5, 6, 0, 99), 0) sameElements Array(30, 1, 1, 4, 2, 5, 6, 0, 99))
    assert(run(Array(1002, 4, 3, 4, 33), 0) sameElements Array(1002, 4, 3, 4, 99))
    assert(run(Array(1101, 100, -1, 4, 0), 0) sameElements Array(1101, 100, -1, 4, 99))
  }
}

object Intcode {
  def apply() = new Intcode
}
