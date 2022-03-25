package aoc.aoc2020

object Day17 {
  import aoc._

  val data: Seq[String] = readFile("aoc2020/Day17.txt").toSeq

  sealed trait Coord {
    def neighbors: Set[Coord]

    def neighbors(s: Set[Coord]): Set[Coord]
  }

  case class Coord3(x: Int, y: Int, z: Int) extends Coord {
    override def neighbors: Set[Coord] = (for {
      xi <- x - 1 to x + 1
      yi <- y - 1 to y + 1
      zi <- z - 1 to z + 1
    } yield Coord3(xi, yi, zi)).filterNot(_ == this).toSet

    override def neighbors(s: Set[Coord]): Set[Coord] = (for {
      xi <- x - 1 to x + 1
      yi <- y - 1 to y + 1
      zi <- z - 1 to z + 1 if s(Coord3(xi,yi,zi))
    } yield Coord3(xi, yi, zi)).filterNot(_ == this).toSet
  }

  case class Coord4(x: Int, y: Int, z: Int, w: Int) extends Coord {
    override def neighbors: Set[Coord] = (for {
      xi <- x - 1 to x + 1
      yi <- y - 1 to y + 1
      zi <- z - 1 to z + 1
      wi <- w - 1 to w + 1
    } yield Coord4(xi, yi, zi, wi)).filterNot(_ == this).toSet

    override def neighbors(s: Set[Coord]): Set[Coord] = (for {
      xi <- x - 1 to x + 1
      yi <- y - 1 to y + 1
      zi <- z - 1 to z + 1
      wi <- w - 1 to w + 1 if s(Coord4(xi,yi,zi, wi))
    } yield Coord4(xi, yi, zi, wi)).filterNot(_ == this).toSet
  }

  //type Coord = (Int, Int, Int)

  def neighbors(c: Coord): Set[Coord] = c.neighbors

//  def neighbors(c: Coord): Set[Coord] = (for {
//    x <- c._1 - 1 to c._1 + 1
//    y <- c._2 - 1 to c._2 + 1
//    z <- c._3 - 1 to c._3 + 1
//  } yield (x, y, z)).filterNot(_ == c).toSet
//
//  def neighbors(c: Coord, s: Set[Coord]): Set[Coord] = (for {
//    x <- c._1 - 1 to c._1 + 1
//    y <- c._2 - 1 to c._2 + 1
//    z <- c._3 - 1 to c._3 + 1 if s((x,y,z))
//  } yield (x, y, z)).filterNot(_ == c).toSet

  def isNeighbor(c1: Coord3, c2: Coord3): Boolean =
    math.abs(c1.x - c2.x) < 2 &&
    math.abs(c1.y - c2.y) < 2 &&
    math.abs(c1.z - c2.z) < 2

  def isNeighbor(c1: Coord4, c2: Coord4): Boolean =
    math.abs(c1.x - c2.x) < 2 &&
    math.abs(c1.y - c2.y) < 2 &&
    math.abs(c1.z - c2.z) < 2 &&
    math.abs(c1.w - c2.w) < 2

  val initialSet3: Set[Coord] = data.zipWithIndex.flatMap{
    d => d._1.zipWithIndex.collect { case ('#', x) => Coord3(d._2, x, 0)}
  }.toSet

  val initialSet4: Set[Coord] = data.zipWithIndex.flatMap{
    d => d._1.zipWithIndex.collect { case ('#', x) => Coord4(d._2, x, 0, 0)}
  }.toSet

  def allNeighbors(s: Set[Coord]): Set[Coord] = s flatMap neighbors

  def becomesActive(c: Coord, as: Set[Coord], ns: Set[Coord]): Boolean = {
    if (as(c)) {
      ns.size == 2 || ns.size == 3
    } else {
      ns.size == 3
    }
  }

  def part1: Int = {
    //idea take allNeighbors, then for each of those, choose only the neighbors that are active and get the count of those

    //val n:Set[Coord] = allNeighbors(initialSet)
    //require (n.intersect(initialSet) == initialSet)

    //val nis = n.map{c => (c, neighbors(c, initialSet))}.collect{case (c, s) if becomesActive(c, initialSet, s) => c}

    iterate[(Set[Coord], Int)]((initialSet3, 0),
      {case(is, cnt) =>
        val n:Set[Coord] = allNeighbors(is)
        val nis = n.map{c => (c, c.neighbors(is))}.collect{case (c, s) if becomesActive(c, is, s) => c}
        (nis, cnt+1)
      }, {case (_, cnt) => cnt == 6}
    )._1.size //242
  }

  def part2: Int = {
    iterate[(Set[Coord], Int)]((initialSet4, 0),
      {case(is, cnt) =>
        val n:Set[Coord] = allNeighbors(is)
        val nis = n.map{c => (c, c.neighbors(is))}.collect{case (c, s) if becomesActive(c, is, s) => c}
        (nis, cnt+1)
      }, {case (_, cnt) => cnt == 6}
    )._1.size
  }
}
