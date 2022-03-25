package aoc.aoc2020

import scala.annotation.tailrec

object Day11 {
  import aoc._

  val board: Seq[String] = readFile("aoc2020/Day11.txt").toSeq

  val ROWS: Int = board.size
  val COLS: Int = board.head.length

  case class BoardIterator(b: Seq[Char], var r: Int, var c: Int, dr: Int, dc: Int) extends Iterator[Char] {
    override def hasNext: Boolean = (0 until ROWS).contains(r) && (0 until COLS).contains(c)

    override def next(): Char = {
      val rc = b(r*COLS+c)
      r += dr
      c += dc
      rc
    }
  }

  def drawBoard(b: Seq[Char]): Unit = {
    for ( i <- b.indices) {
      if ((i+1) % COLS == 0) println(b(i)) else print(b(i))
    }
  }

  def getNeighbors(i: Int, j: Int, board:Seq[Char]): Seq[Char] = {
    for {
      r <- i - 1 to i + 1 if r >= 0 && r < ROWS
      c <- j - 1 to j + 1 if c >= 0 && c < COLS && !(r == i && c == j)
    } yield board(r*COLS+c)
  }

  def getNeighbors2(i: Int, j: Int, board:Seq[Char]): Seq[Char] = {
    val rc = Seq(
      BoardIterator(board, i, j, -1, 0).drop(1).find(_ != '.').getOrElse('.'),
      BoardIterator(board, i, j, 1, 0).drop(1).find(_ != '.').getOrElse('.'),
      BoardIterator(board, i, j, 0, 1).drop(1).find(_ != '.').getOrElse('.'),
      BoardIterator(board, i, j, 0, -1).drop(1).find(_ != '.').getOrElse('.'),
      BoardIterator(board, i, j, -1, -1).drop(1).find(_ != '.').getOrElse('.'),
      BoardIterator(board, i, j, -1, 1).drop(1).find(_ != '.').getOrElse('.'),
      BoardIterator(board, i, j, 1, 1).drop(1).find(_ != '.').getOrElse('.'),
      BoardIterator(board, i, j, 1, -1).drop(1).find(_ != '.').getOrElse('.'))
    //println(s"rc: $rc")
    rc
  }

  def isOccupied(ch: Char): Boolean = ch == '#'

  def newBoard(board: Seq[Char], neighbors: (Int, Int, Seq[Char]) => Seq[Char], numOccupied: Int): Seq[Char] = {
    def newVal(i: Int, j: Int): Char = {
      if (board(i*COLS+j) == '.') '.' else {
      val occupiedCount = neighbors(i, j, board).count(isOccupied)
        board(i*COLS+j) match {
          case 'L' => if (occupiedCount == 0) '#' else 'L'
          case '#' =>if (occupiedCount >= numOccupied) 'L' else '#'
        }
      }
    }

    for {
      i <- 0 until ROWS
      j <- 0 until COLS
    } yield newVal(i, j)
  }

  @tailrec
  def steadyState[T] (v: T, f: T => T): T = {
    val newV = f(v)
    if (newV == v) v else
      steadyState(newV, f)
  }

  def part1: Int = {
    val b = steadyState(board.flatMap(_.toSeq), newBoard(_, getNeighbors, 4))
    //drawBoard(b)
    b.count(isOccupied) //2424
  }

  def part2: Int = {
    val b = steadyState(board.flatMap(_.toSeq), newBoard(_, getNeighbors2, 5))
    drawBoard(b)
    b.count(isOccupied) //2208
  }
}
