package org.romeo.adventofcode
package day1

import common.Puzzle

object DayOne extends App {
  new DayOne().run()
}

class DayOne extends Puzzle(1) {

  def parseInput(input: String): List[Int] = {
    input.split("\n").map(_.toInt).toList
  }


  /**
   * read the problem's input and crash hard if it's invalid
   *
   * @param input
   */
  override def validateInput(input: String): Unit = {
    assert(parseInput(input).forall(_.isInstanceOf[Int]))
  }

  /**
   * solve part 1 of the day's problem
   *
   * @param input
   * @return
   */
  override def solvePart1(input: String): String = {
    val numbers = parseInput(input)
    numbers.toString()
  }

  /**
   * solve part 2 of the day's problem
   *
   * @param input
   * @return
   */
  override def solvePart2(input: String): String = ???
}
