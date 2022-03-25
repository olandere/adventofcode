package aoc.aoc2019

import aoc.readFile1

object Day8 {
  val imageLayers = readFile1("aoc2019/Day8.txt").next().grouped(25 * 6).toSeq
  //val imageLayers = "0222112222120000".grouped(2 * 2).toSeq

  def part1 = {
    val layer = imageLayers.map(_.count(_ == '0')).zipWithIndex.minBy(_._1)._2
    imageLayers(layer).count(_ == '1') * imageLayers(layer).count(_ == '2')
  }

  def combineLayers(l1: String, l2: String): String = {
    l1.zip(l2).map {
      case ('2', x) => x
      case (x, _) => x
    }.mkString
  }

  def printLayer(l: String): Unit = {
    l.map {
      case '0' => ' '
      case '1' => '#'
    }.grouped(25).foreach(println)
  }

  def part2 = {
    val layer = imageLayers.reduceLeft(combineLayers)
    printLayer(layer) // PFCAK
  }
}
