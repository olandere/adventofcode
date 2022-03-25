package aoc.aoc2016

import scala.collection.mutable

object Day11 {

  import aoc._
  
  val layout = readFile1("aoc2016/Day11.txt")
  
  def parse(l: String): Floor = {
    l match {
      case re"The ([a-z]+)$f floor contains ([a-z,\- ]+)$r." => {
        r match {
          case re"nothing relevant" => Floor(f, Set(), Set())
          case _ => {
            val items = r.split(", | and ").map {i =>
              //println(s"got a $i")
              i.trim match {
                case re"(a |and a )$x([a-z]+)$t generator" => Generator(t)
                case re"(a |and a )$x([a-z]+)$t-compatible microchip" => Microchip(t)
              }
            }
            Floor(f, (items.collect{case g:Generator => g}).toSet, (items.collect{case m:Microchip => m}).toSet)
            //fl.chips = (items.collect{case m:Microchip => m}).toSet
            //fl.generators.addAll(items.collect{case g:Generator => g})
            //fl
          }
        }
      }
    }
  }

  case class State(currFloor: Int, floors: Seq[Floor]) {
    def prev = if (currFloor == 0) None else Some(currFloor - 1)
    def next = if (currFloor == floors.size - 1) None else Some(currFloor + 1)

    def nextState = {
      val f = floors(currFloor)

      val (gens, _, _) = canMove(f)
      println(s"gens: $gens")
      next.map{ nf =>
        val nextFloor = floors(nf)
        val p = (for {
          g <- gens
        } yield (floors(currFloor).removeGen(g), nextFloor.addGen(g))).collect{case x if x._2.isDefined => Seq(x._1, x._2.get)}
        p.map{x => State(nf, floors.patch(currFloor, x, 2))}
      }.getOrElse(Set())
      //newFloors.get.map(nf => State(floors.patch(currFloor, nf, 2))
    }

  }

  sealed trait Item(val name: String)
  
  case class Generator(override val name: String) extends Item(name) {
    def isCompatible(m: Microchip) = name == m.name
  }
  
  case class Microchip(override val name: String) extends Item(name) {
    def isCompatible(g: Generator) = name == g.name
  }
  
  case class Floor(id: String, generators: Set[Generator], chips: Set[Microchip]) {
//    val generators = Set[Generator]()
//    val chips = Set[Microchip]()

    def isEmpty = generators.isEmpty && chips.isEmpty

    def canAdd(g: Generator) = {
      chips.isEmpty || chips.exists(_.isCompatible(g)) ||
        chips.forall{c => generators.exists(_.isCompatible(c))}
    }

    def canAdd(c: Microchip) = {
      generators.isEmpty || generators.exists(_.isCompatible(c)) ||
        generators.forall{g => chips.exists(_.isCompatible(g))}
    }

    def addGen(g:Generator) = {
      if (canAdd(g)) Some(Floor(id, generators + g, chips)) else None
    }

    def addGen(gs: Set[Generator]) = {
      if (gs.map(canAdd).foldLeft(true)(_&&_))
        Some(Floor(id, generators ++ gs, chips)) else None
    }

    def removeGen(g:Generator) = {
      Floor(id, generators - g, chips)
    }

    def removeGen(gs: Set[Generator]) = {
      Floor(id, generators -- gs, chips)
    }

    override def toString: String = {
      //s"Floor($id, generators: $generators, chips: $chips)"
      val contents = (generators.map{g => s"${g.name(0).toUpper}G"} ++ chips.map{c => s"${c.name(0).toUpper}M"}).toSeq.sorted
      s"Floor($id, ${contents.mkString(" ")})"
    }

    def unpaired: (Set[Generator], Set[Microchip]) = {
      def remove[T <: Item, T1 <: Item](item: T1, items: Set[T]): Set[T] = {
        items.find(_.name == item.name).map(i => items - i).getOrElse(items)
      }

      def helper[T <: Item, T1 <: Item](ss: Set[T], ds: Set[T1]): Set[T1] = {
        if (ss.isEmpty) ds
        else {
          helper(ss.tail, remove(ss.head, ds))
        }
      }
      (helper(chips, generators), helper(generators, chips))
    }

    def isValid: Boolean = {
      val (g, c) = unpaired
      !(g.nonEmpty && c.nonEmpty)
    }
  }


  def canMove(f: Floor) = {
    val gens = ((for {
      g <- f.generators
    } yield Set((g, Floor("", f.generators - g , f.chips).isValid)).collect{case x if x._2 => x._1})).filter(_.nonEmpty)
    println(s"gens: $gens")
    val pgen = (for {
      g <- f.generators
      g1 <- f.generators if g != g1
    } yield (Set(g, g1), Floor("", f.generators - g - g1, f.chips).isValid)).toSet.collect{case x if x._2 => x._1}
    println(s"pgen: $pgen")
    val chips = (for {
      c <- f.chips
    } yield Set((c, Floor("", f.generators , f.chips - c).isValid)).collect{case x if x._2 => x._1})

    val pchips = (for {
      c <- f.chips
      c1 <- f.chips if c != c1
    } yield (Set(c, c1), Floor("", f.generators , f.chips - c - c1).isValid)).toSet.collect{case x if x._2 => x._1}

    val pairs = (for {
      g <- f.generators
      c <- f.chips if g.isCompatible(c)
    } yield ((g, c), Floor("", f.generators - g , f.chips - c).isValid)).collect{case x if x._2 => x._1}

    (gens ++ pgen, chips ++ pchips, pairs)
  }


  def part1 = {
    State(0, layout.map(parse).toSeq)
  }
}
