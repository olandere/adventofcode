package aoc.aoc2020

import scala.annotation.tailrec

object Day8 {
  import aoc._

  val code: Seq[String] = readFile("aoc2020/Day8.txt").toSeq

  case class State(ip: Int, acc: Int)

  sealed trait Instruction {
    def run(state: State): State
    def swappable: Boolean
    def swap(state: State): State
  }

  case class Acc(arg: Int) extends Instruction {
    override def run(state: State): State = State(state.ip + 1, state.acc + arg)

    override def swappable: Boolean = false

    override def swap(state: State): State = run(state)
  }

  case class Nop(arg: Int) extends Instruction {
    override def run(state: State): State = State(state.ip + 1, state.acc)

    override def swappable: Boolean = true

    override def swap(state: State): State = State(state.ip + arg, state.acc)
  }

  case class Jmp(arg: Int) extends Instruction {
    override def run(state: State): State = State(state.ip + arg, state.acc)

    override def swappable: Boolean = true

    override def swap(state: State): State = State(state.ip + 1, state.acc)
  }

  def parseLine(line: String): Instruction = {
    line match {
      case re"([a-z]{3})$inst ([+-0123456789]+)$op" => inst match {
        case "acc" => Acc(op.toInt)
        case "nop" => Nop(op.toInt)
        case "jmp" => Jmp(op.toInt)
      }
    }
  }

  def runCode(instructions: Seq[Instruction], state: State): State = {
    @tailrec
    def run(state: State, hist: Set[Int]): State = {
      if (hist(state.ip)) state else {
        run(instructions(state.ip).run(state), hist + state.ip)
      }
    }
    run(state, Set.empty)
  }

  def runCode2(instructions: Seq[Instruction], state: State): State = {
    @tailrec
    def runWithSwapped(state: State, hist: Set[Int], swapped: Int): Option[State] = {
      if (hist(state.ip)) None
      else if (state.ip >= instructions.length) Some(state) else {
        if (state.ip == swapped)
          runWithSwapped(instructions(state.ip).swap(state), hist + state.ip, swapped)
        else
          runWithSwapped(instructions(state.ip).run(state), hist + state.ip, swapped)
      }
    }

    def run(state: State, hist: Set[Int]): State = {
      if (hist(state.ip)) state
      else if (state.ip >= instructions.length) state else
      {
        if (instructions(state.ip).swappable)
          runWithSwapped(state, hist, state.ip).getOrElse(run(instructions(state.ip).run(state), hist + state.ip))
        else
          run(instructions(state.ip).run(state), hist + state.ip)
      }
    }

    run(state, Set.empty)
  }

  def part1: Int = {
    runCode(code.map(parseLine), State(0, 0)).acc //1859
  }

  def part2: Int = {
    runCode2(code.map(parseLine), State(0, 0)).acc //1235
  }
}
