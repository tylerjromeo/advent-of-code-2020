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
   * @param input the string of input coming from advent of code
   */
  override def validateInput(input: String): Unit = {
    assert(parseInput(input).forall(_.isInstanceOf[Int]))
  }

  /**
   * solve part 1 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart1(input: String): String = {
    val numbers = parseInput(input)
    val numbersToMultiply = numbers.combinations(2).find(_.sum == 2020)
    numbersToMultiply match {
      case None => "FAILED TO FIND MATCH"
      case Some(l) => l.product.toString
    }
  }

  /**
   * solve part 2 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart2(input: String): String = {
    val numbers = parseInput(input)
    val numbersToMultiply = numbers.combinations(3).find(_.sum == 2020)
    numbersToMultiply match {
      case None => "FAILED TO FIND MATCH"
      case Some(l) => l.product.toString
    }
  }
}
