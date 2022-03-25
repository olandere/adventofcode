package aoc.aoc2015

object Day9 {

  import aoc._

  val distances = readFile1("aoc2015/Day9.txt").map {
    case re"(\w+)$s to (\w+)$d = (\d+)$v" => Key(s, d) -> v.toInt
  }.toMap

  val destinations = distances.keys.flatMap(_.toSet).toSeq

  case class Key(s: String, d: String) {
    def toSet = Set(s, d)
  }

  case object Key {
    def apply(s: String, d: String): Key = {
      if (s < d) new Key(s, d) else new Key(d, s)
    }
  }

  def pathLength(s: Seq[String]): Int =
    s match {
      case Seq(a, b, _*) => distances(Key(a, b)) + pathLength(s.tail)
      case Seq(a) => 0
    }

  def part1 = {
    // brute force solution
    // todo: try greedy alg.
    destinations.permutations.map(p => p -> pathLength(p)).minBy(_._2) //141
  }

  def part2 = {
    destinations.permutations.map(p => p -> pathLength(p)).maxBy(_._2) //736
  }
}
