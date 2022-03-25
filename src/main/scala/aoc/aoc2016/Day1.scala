package aoc.aoc2016

object Day1 {

  import aoc._

  val directions: Seq[String] = readFile1("aoc2016/Day1.txt").next().split(", ").toSeq
  //val directions: Seq[String] = Seq("R8", "R4", "R4", "R8")

  type Coord = (Int, Int)

  sealed trait Direction {
    def left(start: Coord, d: Int): (Direction, Coord)

    def right(start: Coord, d: Int): (Direction, Coord)

    def move(start: Coord, dir: String): (Direction, (Int, Int)) =
      dir match {
        case re"L(\d+)$d" => left(start, d.toInt)
        case re"R(\d+)$d" => right(start, d.toInt)
      }
  }

  case object North extends Direction {
    override def left(start: (Int, Int), d: Int): (Direction, Coord) = {
      (West, (start._1 - d, start._2))
    }

    override def right(start: (Int, Int), d: Int): (Direction, Coord) = {
      (East, (start._1 + d, start._2))
    }
  }

  case object South extends Direction {
    override def left(start: (Int, Int), d: Int): (Direction, (Int, Int)) = (East, (start._1 + d, start._2))

    override def right(start: (Int, Int), d: Int): (Direction, (Int, Int)) = (West, (start._1 - d, start._2))
  }

  case object East extends Direction {
    override def left(start: (Int, Int), d: Int): (Direction, (Int, Int)) = (North, (start._1, start._2 + d))

    override def right(start: (Int, Int), d: Int): (Direction, (Int, Int)) = (South, (start._1, start._2 - d))
  }

  case object West extends Direction {
    override def left(start: (Int, Int), d: Int): (Direction, (Int, Int)) = (South, (start._1, start._2 - d))

    override def right(start: (Int, Int), d: Int): (Direction, (Int, Int)) = (North, (start._1, start._2 + d))
  }

  def move(t: (Direction, Coord, Seq[String])): (Direction, Coord, Seq[String]) = {
    val (dir, start) = t._1.move(t._2, t._3.head)
    (dir, start, t._3.tail)
  }

  def moveWithAcc(t: (Direction, Segment, Seq[String], Seq[Segment])): (Direction, Segment, Seq[String], Seq[Segment]) = {
    val (dir, start) = t._1.move(t._2.e2, t._3.head)
    (dir, Segment(t._2.e2, start), t._3.tail, t._4 :+ t._2)
  }

  case class Segment(e1: Coord, e2: Coord) {
    val slope: Option[Int] = if (e2._1 - e1._1 == 0) None else Some(0)  //only have horizontal or vertical lines

    def intersect(that: Segment): Option[Coord] = {
      if (this.e2 == that.e1) None //
      else {
        if (this.slope == that.slope) None //Parallel lines
        else {
          val x = if (this.e1._1== this.e2._1) this.e1._1 else that.e1._1
          val y = if (this.e1._2== this.e2._2) this.e1._2 else that.e1._2
          if (this.contains((x, y)) && that.contains((x, y))) Some((x, y)) else None
        }
      }
    }

    override
    def toString() = {
      if (slope.isDefined) {
        s"y = ${e1._2}; [$e1, $e2]"
      } else s"x = ${e1._1}; [$e1, $e2]"
    }

    def contains(c: Coord): Boolean = {
      val rx: Range = math.min(e1._1, e2._1) to math.max(e1._1, e2._1)
      val ry: Range  = math.min(e1._2, e2._2) to math.max(e1._2, e2._2)
      (rx contains c._1) && (ry contains c._2)
    }
  }

  def dist(c: Coord): Int = math.abs(c._1) + math.abs(c._2)

  def part1: Int = {
    val (_, dest, _) = iterate((North, (0, 0), directions),
      move,
      (t: (_, _, Seq[_])) => t._3.isEmpty
    )
    math.abs(dest._1) + math.abs(dest._2) //250
  }

  def part2: Int = {
    val (_, dest, _, acc) = iterate((North, Segment((0, 0), (0,0)), directions, Seq()),
      moveWithAcc,
      (t: (_, Segment, Seq[_], Seq[Segment])) => t._4.exists{p => p.intersect(t._2).isDefined}//t._3.isEmpty//t._4(t._2)
    )
    val p = acc.filter(p => p.intersect(dest).isDefined).flatMap(_.intersect(dest))
    dist(p.head) //151
  }
}
