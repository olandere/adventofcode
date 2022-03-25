package aoc.aoc2021

import aoc.aoc2021.Day4.{Board, boards}
import aoc.readFile1

object Day4 {

  val input = readFile1("aoc2021/Day4.txt")

  val draws = input.next().split(',').map(_.toInt)

  val boards = Board.readBoards(input)

  def score(idx: Int, board: Board): Int = {
    val drawnVals = draws.take(idx)
    val lastDraw = drawnVals.last
    board.grid.toSet.diff(drawnVals.toSet).sum * lastDraw
  }

  def part1 = {
    def helper(iter: Int): (Int, Board) = {
      val b = boards.find(_.isSolved(draws.take(iter).toSet))
      if (b.isDefined) (iter, b.get)
      else
        helper(iter + 1)
    }

    score _ tupled(helper(5)) //65325
  }

  def part2 = {
    def solvedBoards(idx: Int, boards: Seq[Board], solved: Seq[(Int, Seq[Board])]): Seq[(Int, Seq[Board])] = {
      if (idx >= draws.size) solved
      else {
        val bs = boards.filter(_.isSolved(draws.take(idx).toSet))
        if (bs.nonEmpty) solvedBoards(idx + 1, boards.filterNot(b=> bs.contains(b)), solved :+ (idx, bs))
        else
          solvedBoards(idx + 1, boards, solved)
      }
    }
    val (idx, board) = solvedBoards(5, boards, Seq()).last
    score(idx, board.head) //4624
  }

  class Board(val grid: Seq[Int]) {

    val rows = grid.sliding(5, 5).map(_.toSet).toSeq
    val cols = grid.tails.take(5).map(_.sliding(1, 5).toSet.flatten).toSeq

    def hasMarkedRow(drawn: Set[Int]): Boolean = {
      rows.exists { s => s.diff(drawn).isEmpty }
    }

    def hasMarkedCol(drawn: Set[Int]): Boolean = {
      cols.exists { s => s.diff(drawn).isEmpty }
    }

    def isSolved(drawn: Set[Int]): Boolean = hasMarkedRow(drawn) || hasMarkedCol(drawn)
  }

  object Board {
    def apply(l: Seq[String]) = {
      new Board(l.flatMap(_.trim.split("[ ]+").map(_.toInt)))
    }

    def readBoards(lines: Iterator[String]): Seq[Board] = {
      if (lines.isEmpty) Seq() else
        Seq(Board(lines.dropWhile(_.isEmpty).take(5).toSeq)) ++ readBoards(lines)
    }
  }

}
