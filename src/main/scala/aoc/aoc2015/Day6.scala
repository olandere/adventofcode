package aoc.aoc2015

object Day6 {

  import aoc._

  val lights: Array[Array[Boolean]] = Array.fill(1000, 1000){false}
  val intLights: Array[Array[Int]] = Array.fill(1000, 1000){0}

  val instructions: Seq[Inst] = readFile("aoc2015/Day6.txt").map(processInstruction).toSeq

  abstract class Inst(x1: Int, y1: Int, x2: Int, y2: Int) {
    def newState(b: Boolean): Boolean
    def newIntState(i: Int): Int

    def update(l: Array[Array[Boolean]]): Array[Array[Boolean]] = {
      for (
        x <- x1 to x2;
        y <- y1 to y2)
        l(x)(y) = newState(l(x)(y))
      l
    }

    def update(l: Array[Array[Int]]): Array[Array[Int]] = {
      for (
        x <- x1 to x2;
        y <- y1 to y2)
        l(x)(y) = newIntState(l(x)(y))
      l
    }
  }

  case class On(x1: Int, y1: Int, x2: Int, y2: Int) extends Inst(x1, y1, x2, y2) {
    override def newState(b: Boolean): Boolean = true

    override def newIntState(i: Int): Int = i + 1
  }

  case class Off(x1: Int, y1: Int, x2: Int, y2: Int) extends Inst(x1, y1, x2, y2) {
    override def newState(b: Boolean): Boolean = false

    override def newIntState(i: Int): Int = math.max(0, i - 1)
  }

  case class Toggle(x1: Int, y1: Int, x2: Int, y2: Int) extends Inst(x1, y1, x2, y2) {
    override def newState(b: Boolean): Boolean = !b

    override def newIntState(i: Int): Int = i + 2
  }

  def processInstruction(str: String): Inst = {
    str match {
      case re"turn on (\d+)$x1,(\d+)$y1 through (\d+)$x2,(\d+)$y2" => On(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      case re"turn off (\d+)$x1,(\d+)$y1 through (\d+)$x2,(\d+)$y2" => Off(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      case re"toggle (\d+)$x1,(\d+)$y1 through (\d+)$x2,(\d+)$y2" => Toggle(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
  }

  def part1: Int = {
    instructions.foreach(_.update(lights))
    lights.map(_.count(_ == true)).sum //543903
  }

  def part2: Int = {
    instructions.foreach(_.update(intLights))
    intLights.map(_.sum).sum
  }
}
