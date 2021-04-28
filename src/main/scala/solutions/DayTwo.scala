package org.romeo.adventofcode
package solutions

import common.Puzzle

object DayTwo extends App {
  new DayTwo().run()
}

class DayTwo extends Puzzle(2) {

  case class PasswordWithPolicy(
                                 password: String,
                                 specialCharacter: Char,
                                 minOccurrences: Int,
                                 maxOccurrences: Int
                               ) {
    def isValid: Boolean = {
      val occurrences = password.count(_ == specialCharacter)
      minOccurrences <= occurrences && occurrences <= maxOccurrences
    }

    def isValid_secondInterpretation: Boolean = {
      val check1 = password(minOccurrences - 1)
      val check2 = password(maxOccurrences - 1)

      (check1 == specialCharacter) != (check2 == specialCharacter) // use != as exclusive or
    }
  }

  def passwordWithPolicyFromString(s: String): PasswordWithPolicy = {
    val split = s.split(" ")
    val range = split(0).split("-")
    val specialCharacter = split(1).head
    val password = split(2)
    PasswordWithPolicy(password, specialCharacter, range(0).toInt, range(1).toInt)

  }

  def parseInput(input: String): List[PasswordWithPolicy] = {
    input.split("\n").map(passwordWithPolicyFromString).toList
  }

  /**
   * read the problem's input and crash hard if it's invalid
   *
   * @param input the string of input coming from advent of code
   */
  override def validateInput(input: String): Unit = {
    assert(parseInput(input).forall(_.isInstanceOf[PasswordWithPolicy]))
  }

  /**
   * solve part 1 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart1(input: String): String = {
    val policies = parseInput(input)
    policies.count(_.isValid).toString
  }

  /**
   * solve part 2 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart2(input: String): String = {
    val policies = parseInput(input)
    policies.count(_.isValid_secondInterpretation).toString
  }
}
