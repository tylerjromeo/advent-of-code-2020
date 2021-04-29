package org.romeo.adventofcode
package solutions

import common.Puzzle

object DayThree extends App {
  new DayThree().run()
}

class DayThree extends Puzzle(3) {

  sealed trait Cell
  case object Tree extends Cell
  case object Clear extends Cell


  def parseInput(input: String): List[List[Cell]] = {
      input.split("\n").toList.map(row => {
        row.map{
          case '#' => Tree
          case _ => Clear
        }.toList
      })
  }


  /**
   * read the problem's input and crash hard if it's invalid
   *
   * @param input the string of input coming from advent of code
   */
  override def validateInput(input: String): Unit = {
    parseInput(input).flatten.foreach(_.isInstanceOf[Cell])
  }

  def calculateTreesHit(right: Int, cells: List[List[Cell]]): Int = {
    val width = cells.head.length
    cells.foldLeft((0, 0)) {
      case ((rightOffset, totalTrees), row) =>
        val treeHit = row(rightOffset) match {
          case Tree => 1
          case Clear => 0
        }
        ((rightOffset + right) % width, totalTrees + treeHit)
    }._2
  }
  /**
   * solve part 1 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart1(input: String): String = {
    calculateTreesHit(3, parseInput(input)).toString
  }

  /**
   * solve part 2 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart2(input: String): String = {
    val cells = parseInput(input)
    val slope1_1 = calculateTreesHit(1, cells)
    val slope3_1 = calculateTreesHit(3, cells)
    val slope5_1 = calculateTreesHit(5, cells)
    val slope7_1 = calculateTreesHit(7, cells)

    //go down 2 by taking every other row
    val everyOtherRow = cells.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val slope1_2 = calculateTreesHit(1, everyOtherRow)
    (slope1_1 * slope3_1 * slope5_1 * slope7_1 * slope1_2).toString
  }
}
