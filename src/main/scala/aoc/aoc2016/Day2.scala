package aoc.aoc2016

object Day2 {

  import aoc._

  val directions: Seq[String] = readFile1("aoc2016/Day2.txt").toSeq

  sealed trait Key {
    def left: Key

    def right: Key

    def up: Key

    def down: Key

    def move(dir: Char): Key = {
      dir match {
        case 'L' => left
        case 'R' => right
        case 'U' => up
        case 'D' => down
      }
    }
  }

  case object NineKeyPad {

    case object One extends Key {
      override def left: Key = this

      override def right: Key = Two

      override def up: Key = this

      override def down: Key = Four

      override def toString: String = "1"
    }

    case object Two extends Key {
      override def left: Key = One

      override def right: Key = Three

      override def up: Key = this

      override def down: Key = Five

      override def toString: String = "2"
    }

    case object Three extends Key {
      override def left: Key = Two

      override def right: Key = this

      override def up: Key = this

      override def down: Key = Six

      override def toString: String = "3"
    }

    case object Four extends Key {
      override def left: Key = this

      override def right: Key = Five

      override def up: Key = One

      override def down: Key = Seven

      override def toString: String = "4"
    }

    case object Five extends Key {
      override def left: Key = Four

      override def right: Key = Six

      override def up: Key = Two

      override def down: Key = Eight

      override def toString: String = "5"
    }

    case object Six extends Key {
      override def left: Key = Five

      override def right: Key = this

      override def up: Key = Three

      override def down: Key = Nine

      override def toString: String = "6"
    }

    case object Seven extends Key {

      override def left: Key = this

      override def right: Key = Eight

      override def up: Key = Four

      override def down: Key = this

      override def toString: String = "7"
    }

    case object Eight extends Key {
      override def left: Key = Seven

      override def right: Key = Nine

      override def up: Key = Five

      override def down: Key = this

      override def toString: String = "8"
    }

    case object Nine extends Key {
      override def left: Key = Eight

      override def right: Key = this

      override def up: Key = Six

      override def down: Key = this

      override def toString: String = "9"
    }
  }

  case object HexKeyPad {

    case object One extends Key {
      override def left: Key = this

      override def right: Key = this

      override def up: Key = this

      override def down: Key = Three

      override def toString: String = "1"
    }

    case object Two extends Key {
      override def left: Key = this

      override def right: Key = Three

      override def up: Key = this

      override def down: Key = Six

      override def toString: String = "2"
    }

    case object Three extends Key {
      override def left: Key = Two

      override def right: Key = Four

      override def up: Key = One

      override def down: Key = Seven

      override def toString: String = "3"
    }

    case object Four extends Key {
      override def left: Key = Three

      override def right: Key = this

      override def up: Key = this

      override def down: Key = Eight

      override def toString: String = "4"
    }

    case object Five extends Key {
      override def left: Key = this

      override def right: Key = Six

      override def up: Key = this

      override def down: Key = this

      override def toString: String = "5"
    }

    case object Six extends Key {
      override def left: Key = Five

      override def right: Key = Seven

      override def up: Key = Two

      override def down: Key = AKey

      override def toString: String = "6"
    }

    case object Seven extends Key {

      override def left: Key = Six

      override def right: Key = Eight

      override def up: Key = Three

      override def down: Key = BKey

      override def toString: String = "7"
    }

    case object Eight extends Key {
      override def left: Key = Seven

      override def right: Key = Nine

      override def up: Key = Four

      override def down: Key = CKey

      override def toString: String = "8"
    }

    case object Nine extends Key {
      override def left: Key = Eight

      override def right: Key = this

      override def up: Key = this

      override def down: Key = this

      override def toString: String = "9"
    }

    case object AKey extends Key {
      override def left: Key = this

      override def right: Key = BKey

      override def up: Key = Six

      override def down: Key = this

      override def toString: String = "A"
    }

    case object BKey extends Key {
      override def left: Key = AKey

      override def right: Key = CKey

      override def up: Key = Seven

      override def down: Key = DKey

      override def toString: String = "B"
    }

    case object CKey extends Key {
      override def left: Key = BKey

      override def right: Key = this

      override def up: Key = Eight

      override def down: Key = this

      override def toString: String = "C"
    }

    case object DKey extends Key {
      override def left: Key = this

      override def right: Key = this

      override def up: Key = BKey

      override def down: Key = this

      override def toString: String = "D"
    }
  }

  def doMove(k: Key, ch: Char): Key = k.move(ch)

  def getCode(moves: Seq[String], startKey: Key): String =
    moves.scanLeft[Key](startKey) { (k, d) =>
      d.foldLeft(k)(doMove)
    }.tail.mkString

  def part1: String = {
    getCode(directions, NineKeyPad.Five) //92435
  }

  def part2: String = {
    getCode(directions, HexKeyPad.Five) //C1A88
  }
}