package aoc.aoc2016

object Day8 {

  import aoc._

  type Board = Array[Array[Char]]

  val instructions = readFile1("aoc2016/Day8.txt").map(parse).toSeq

  val board: Board = Array.fill(6, 50) {
    '.'
  }

  def show = board.foreach(r => println(r.mkString))

  def parse(inst: String): Instruction = {
    inst match {
      case re"rect (\d+)$x[x](\d+)$y" => Rect(x.toInt, y.toInt)
      case re"rotate row y=(\d+)$y by (\d+)$d" => RotateRow(y.toInt, d.toInt)
      case re"rotate column x=(\d+)$x by (\d+)$d" => RotateCol(x.toInt, d.toInt)
    }
  }

  sealed trait Instruction {
    def process(b: Board): Board
  }

  case class Rect(x: Int, y: Int) extends Instruction {
    def process(b: Board): Board = {
      for {
        i <- 0 until x
        j <- 0 until y
      } (b(j).update(i, '#'))
      b
    }
  }

  case class RotateRow(r: Int, d: Int) extends Instruction {
    override def process(b: Board): Board = {
      def rotate1(arr: Array[Char]) = {
        val last = arr(arr.length - 1)
        for {
          i <- arr.length - 1 to 1 by -1
        } arr(i) = arr(i - 1)
        arr(0) = last
      }

      for (i <- 1 to d)
        rotate1(b(r))
      b
    }
  }

  case class RotateCol(c: Int, d: Int) extends Instruction {
    override def process(b: Board): Board = {
      def rotate1 = {
        val last = b(b.length - 1)(c)
        for {
          i <- b.length - 1 to 1 by -1
        } b(i)(c) = b(i - 1)(c)
        b(0)(c) = last
      }

      for (i <- 1 to d)
        rotate1
      b
    }

  }
  
  def part1 = {
    show
    synchronized(
    instructions.foreach{i => i.process(board);show;println();wait(200)}
    )
    board.map(_.count{c => c =='#'}).sum //123
  }
  
  def part2 = show //AFBUPZBJPS

}