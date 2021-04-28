package org.romeo.adventofcode
package common

abstract class Puzzle(day: Int) {

  def run(): Unit = {
    WebInputReader.getInput(day) match {
      case Left(e) => println(s"Failed: $e")
      case Right(input) =>
        validateInput(input)
        println(s"Part one's answer is ${solvePart1(input)}")
        println(s"Part two's answer is ${solvePart2(input)}")
    }
  }


  /**
   * read the problem's input and crash hard if it's invalid
   *
   * @param input the string of input coming from advent of code
   */
  def validateInput(input: String): Unit

  /**
   * solve part 1 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  def solvePart1(input: String): String

  /**
   * solve part 2 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  def solvePart2(input: String): String


}
