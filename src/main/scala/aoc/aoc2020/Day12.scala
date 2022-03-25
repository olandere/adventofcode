package aoc.aoc2020

import scala.annotation.tailrec

object Day12 {
  import aoc._

  val directions: Seq[String] = readFile("aoc2020/Day12.txt").toSeq

  sealed trait Direction {
    var dist: Int = 0

    def move(d: Int): Unit = {
      dist += d
    }

    def left: Direction

    def right: Direction
  }

  sealed trait Turn

  case object Left extends Turn

  case object Right extends Turn


  case object Ferry {
    def init(): Unit = {
      North.dist = 0
      South.dist = 0
      East.dist = 0
      West.dist = 0
    }

    def move(d: Int): Unit = {
      North.move(d*Waypoint.North.dist)
      South.move(d*Waypoint.South.dist)
      East.move(d*Waypoint.East.dist)
      West.move(d*Waypoint.West.dist)
    }

    case object North extends Direction {
      override def left: Direction = West

      override def right: Direction = East
    }

    case object South extends Direction {
      override def left: Direction = East

      override def right: Direction = West
    }

    case object East extends Direction {
      override def left: Direction = North

      override def right: Direction = South
    }

    case object West extends Direction {
      override def left: Direction = South

      override def right: Direction = North
    }

    case object Heading {
      def turnRight(d: Int): Unit = facing = d match {
        case 90 => facing.right
        case 180 => facing.right.right
        case 270 => facing.left
      }

      def turnLeft(d: Int): Unit = facing = d match {
        case 90 => facing.left
        case 180 => facing.left.left
        case 270 => facing.right
      }

      var facing: Direction = East

      def move(d: Int): Unit = {
        facing.move(d)
      }
    }

    def processLine(str: String): Unit = {
      str match {
        case re"([NSEWFRL])$d(\d+)$n" =>
          d match {
            case "N" => North.move(n.toInt)
            case "S" => South.move(n.toInt)
            case "E" => East.move(n.toInt)
            case "W" => West.move(n.toInt)
            case "F" => Heading.move(n.toInt)
            case "L" => Heading.turnLeft(n.toInt)
            case "R" => Heading.turnRight(n.toInt)
          }
      }
    }

    def processLine2(str: String): Unit = {
      str match {
        case re"([NSEWFRL])$d(\d+)$n" =>
          d match {
            case "N" => Waypoint.North.move(n.toInt)
            case "S" => Waypoint.South.move(n.toInt)
            case "E" => Waypoint.East.move(n.toInt)
            case "W" => Waypoint.West.move(n.toInt)
            case "F" => Ferry.move(n.toInt)
            case "L" => Waypoint.rotateLeft(n.toInt)
            case "R" => Waypoint.rotateRight(n.toInt)
          }
      }
    }

    case object Waypoint {
      @tailrec
      def rotateRight(deg: Int):Unit = {
        if (deg > 0) {
          val n = North.dist
          North.dist = West.dist
          West.dist = South.dist
          South.dist = East.dist
          East.dist = n
          rotateRight(deg-90)
        }
      }

      def normalize(): Unit = {
        if (North.dist > South.dist) {
          North.move(-South.dist)
          South.move(-South.dist)
        } else {
          South.move(-North.dist)
          North.move(-North.dist)
        }
        if (East.dist > West.dist) {
          East.move(-West.dist)
          West.move(-West.dist)
        } else {
          West.move(-East.dist)
          East.move(-East.dist)
        }
      }

      def rotateLeft(deg: Int): Unit = rotateRight(360-deg)

      //Initial position
      def init(): Unit = {
        North.dist = 1
        East.dist = 10
        South.dist = 0
        West.dist = 0
      }

      case object North extends Direction {
        override def left: Direction = West

        override def right: Direction = East
      }

      case object South extends Direction {
        override def left: Direction = East

        override def right: Direction = West
      }

      case object East extends Direction {
        override def left: Direction = North

        override def right: Direction = South
      }

      case object West extends Direction {
        override def left: Direction = South

        override def right: Direction = North
      }

    }

    def manhattenDistance: Int = math.abs(North.dist - South.dist) + math.abs(West.dist - East.dist)
  }

  def part1: Int = {
    Ferry.init()
    directions.foreach(Ferry.processLine)
    Ferry.manhattenDistance //1956
  }

  def part2: Int = {
    Ferry.init()
    Ferry.Waypoint.init()
    directions.foreach(Ferry.processLine2)
    Ferry.manhattenDistance //126797

  }

}
