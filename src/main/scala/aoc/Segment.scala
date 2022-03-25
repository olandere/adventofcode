package aoc

case class Segment(x1: Int, y1: Int, x2: Int, y2: Int) {

//  def normalize: Segment = {
//    if (isHorizontal) {
//      if (x1 < x2) this else Segment(x2, y2, x1, y1)
//    } else if (isVertical) {
//      if (y1 < y2) this else Segment(x2, y2, x1, y1)
//    } else if (x1 < x2) this else Segment(x2, y2, x1, y1)
//  }

  def isHorizontal = y1 == y2

  def isVertical = x1 == x2

  def isHorzOrVert = isHorizontal || isVertical

  def slope = (y2 - y1, x2 - x1)

  val rx: Range = math.min(x1, x2) to math.max(x1, x2)
  val ry: Range  = math.min(y1, y2) to math.max(y1, y2)

  def contains(c: (Int, Int)): Boolean = {
    //println(s"is $c in $this")
    (rx contains c._1) && (ry contains c._2)
  }

  def length = {
    if (isHorizontal) Math.abs(x1 - x2)
    else Math.abs(y1 - y2)
  }

  def toPoints: Set[(Int, Int)] = {
    if (isHorzOrVert)
      (for {i <- rx
            j <- ry} yield (i, j)).toSet
    else {
      Set(rx.zip(if (y1 < y2) y1 to y2 else y1 to y2 by -1)).flatten
    }
  }

  def intersect(s2: Segment) = {
    if (Math.min(x1, x2) > Math.max(s2.x1, s2.x2)) Set()
    else if (Math.max(x1, x2) < Math.min(s2.x1, s2.x2)) Set()
    else if (Math.min(y1, y2) > Math.max(s2.y1, s2.y2)) Set()
    else if (Math.max(y1, y2) < Math.min(s2.y1, s2.y2)) Set()
    else
      toPoints.intersect(s2.toPoints)
  }

  def overlap(s2: Segment) = {
    if (Math.min(x1, x2) > Math.max(s2.x1, s2.x2)) None
    else if (Math.max(x1, x2) < Math.min(s2.x1, s2.x2)) None
    else if (Math.min(y1, y2) > Math.max(s2.y1, s2.y2)) None
    else if (Math.max(y1, y2) < Math.min(s2.y1, s2.y2)) None
    else if (isHorizontal != s2.isHorizontal) {
      if (isHorizontal) Some(Set((s2.x1, y1))) else Some(Set((x1, s2.y1)))
    } // perpendicular lines
    else {
      Option(Segment(Math.max(x1, s2.x1), Math.max(y1, s2.y1), Math.min(x2, s2.x2), Math.min(y2, s2.y2)).toPoints.toSet)
    }
  }
}

object Segment {
  def apply(x1: Int, y1: Int, x2: Int, y2: Int): Segment = {
    new Segment(x1, y1, x2, y2) //.normalize
  }
}

