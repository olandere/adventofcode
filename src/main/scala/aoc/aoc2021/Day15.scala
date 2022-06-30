package aoc.aoc2021

import aoc.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day15 {

  import math.Ordering.Implicits.infixOrderingOps

  type Board = Seq[Seq[Int]]

  val board: Board = readFile1("aoc2021/Day15.txt").map(_.map(c => c.asDigit)).toSeq
 //  val board = brd//reverse(resize(brd, 9))
  //val board = reverse(brd)

  val bx: Int = board.size
  val by: Int = board.head.size


  var cnt = 0

  def reverse(b: Board): Board = b.map(_.reverse).reverse

  def resize(b: Board, n: Int): Seq[Seq[Int]] = b.map(_.take(n)).take(n)

  case class Path(x: Int, y: Int, weight: Int) extends Comparable[Path] {
    //val visited: mutable.Map[(Int, Int), Int] = mutable.HashMap()

    def dist: Int = x + y

    override def compareTo(that: Path): Int = {
//      val z = weight.compareTo(that.weight)
//      if (z == 0) -(this.dist.compareTo(that.dist))
//      else z
      weight.compareTo(that.weight)
    }

    override def equals(that: Any): Boolean = {
      that match
        case that: Path => this.key == that.key
        case _ => false
    }

    override def hashCode(): Int = key.hashCode()

    def key: (Int, Int) = (x, y)


 //   def wasVisited(p: Path) =
 //     visited(p.key)

//    def addVisit(p: Path): Option[Path] = {
//        if (!visited.contains(p.key) || visited(p.key) > p.weight) {
//          visited(p.key) = p.weight
//          Some(p)
//        } else {
//          None
//        }
//    }
  }

  def newPaths(board: (Int, Int) => Int, p: Path, offset: Int): Seq[Path] = {
    def newPath(x: Int, y: Int, p: Path): Option[Path] = {
     // println(s"($x, $y), p: $p")
     // if (p.visited((x, y))) None
     // else {
        Some(Path(x, y, p.weight + board(x, y))) //, p.visited + ((x, y))))
      //}
    }

    Seq(
      if ((p.x + 1) < bx*offset) newPath(p.x + 1, p.y, p) else None,
      if ((p.y + 1) < by*offset) newPath(p.x, p.y + 1, p) else None,
      if ((p.x - 1) >= 0) newPath(p.x - 1, p.y, p) else None,
      if ((p.y - 1) >= 0) newPath(p.x, p.y - 1, p) else None
    ).collect { case Some(p: Path) => p }.sorted
    //np.map(p=>p.addVisit(p))
    //addVisit(np)
    // np
  }

  class PathWeights(board: Board) {
    val weights: Seq[mutable.Seq[Int]] = Seq(mutable.Seq.empty)

    def firstRow: Seq[Int] = board.head.tail.scan(0)(_ + _)

    def nextRow(wp: Seq[Int], br: Seq[Int]) = {
      br.zip(wp).tail.scanLeft(br(0) + wp(0))((p, t) => math.min(p, t._2) + t._1)
    }

    def compute(): Seq[Int] = {
      board.tail.fold(firstRow)(nextRow)
      //b.zip(w).scan{case(i, j) => math.min(i, j)}
      //      for {
      //        i <- board.indices
      //        j <- board(i).indices
      //      } yield (i, j)
    }
  }


  def merge[A: Ordering](as: Seq[A], bs: Seq[A]): Seq[A] = {

    @tailrec
    def helper(as: Seq[A], bs: Seq[A], acc: Seq[A]): Seq[A] = {
      if (as.isEmpty) acc ++ bs
      else if (bs.isEmpty) acc ++ as
      else if (as.head < bs.head) helper(as.tail, bs, acc :+ as.head)
      else if (as.head == bs.head) helper(as.tail, bs.tail, acc :+ as.head)
      else helper(as, bs.tail, acc :+ bs.head)
    }

    helper(as, bs, Nil)
  }

//  def extendPath(paths: Seq[Path]): Seq[Path] = {
//    //println(paths)
//    cnt += 1
//    merge(newPaths(paths.head), paths.tail)
//  }

  def leastCostPaths(board: (Int, Int) => Int, dest: Path, offset: Int): Int = {
    val weights: mutable.Map[(Int, Int), Int] = new mutable.HashMap()
      //Seq[mutable.Seq[Option[Int]]] = Seq(mutable.Seq.empty) // shortest paths so far

    val pq = new java.util.PriorityQueue[Path]() //track cells that need computed or recomputed
    val start = Path(0, 0, 0)
    weights(start.key) = 0
    pq.add(start)
    var cnt = 0
    while(!pq.isEmpty && !weights.contains(dest.key)) {
      val (in, out) = newPaths(board, pq.poll(), offset).partition(p => weights.contains(p.key))
 //     println(s"in: $in, out: $out, weights: $weights")
      in.foreach{p => if (weights(p.key) > p.weight) {
        weights(p.key) = p.weight
  //      println(s"before $pq")
        pq.remove(p)
        pq.add(p)
   //     println(s"after $pq")
      }}
      out.foreach{p => weights(p.key) = p.weight
        pq.add(p)
      }
      cnt += 1
    }
    println(s"cnt: $cnt")
    weights(dest.key)
  }

  def isDone(paths: Seq[Path]): Boolean =
    paths.head.key == (bx - 1, by - 1)

  def tiledBoard(board: Board, mx: Int, my: Int)(x: Int, y: Int): Int = {
    val nx = x % bx
    val ny = y % by
    val offset = x / bx + y / by
    (board(nx)(ny) - 1 + offset) % 9 + 1
  }

  def part1: Int = {
    //val paths = Seq(Path(0, 0, 0, Set[(Int, Int)]()))
  //  val result = iterate(paths, extendPath, isDone)
   // println(result)
    //result.head.weight
    def boardLookup(x: Int, y: Int) = board(x)(y)
    leastCostPaths(boardLookup, Path(bx - 1, by - 1, 0), 1) //527
  }

  def part2 = {
    val tb = tiledBoard(board, 5, 5)
    leastCostPaths(tb, Path(bx*5 - 1, by*5 - 1, 0), 5)
  }
}
