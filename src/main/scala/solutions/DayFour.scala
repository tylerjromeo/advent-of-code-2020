package org.romeo.adventofcode
package solutions

import common.Puzzle

object DayFour extends App {
  new DayFour().run()
}

class DayFour extends Puzzle(4) {

  case class Passport(
                       birthYear: Option[Int],
                       issueYear: Option[Int],
                       expirationYear: Option[Int],
                       height: Option[String],
                       hairColor: Option[String],
                       eyeColor: Option[String],
                       passportId: Option[String],
                       countryId: Option[String]
                     ) {
    def isValid: Boolean = {
      birthYear.nonEmpty && issueYear.nonEmpty && expirationYear.nonEmpty && height.nonEmpty && hairColor.nonEmpty && eyeColor.nonEmpty && passportId.nonEmpty
    }

    def isValid_withFields: Boolean = {
      isValid &&
        birthYear.forall(y => y >= 1920 && y <= 2002) &&
        issueYear.forall(y => y >= 2010 && y <= 2020) &&
        expirationYear.forall(y => y >= 2020 && y <= 2030) &&
        height.forall(isValidHeight) &&
        hairColor.forall(isValidHairColor) &&
        eyeColor.forall(isValidEyeColor) &&
        passportId.forall(isValidPassport)

    }
  }

  def isValidHeight(s: String): Boolean = {
    val heightPattern = "(\\d+)(cm|in)".r
    s match {
      case heightPattern(n, unit) if unit == "cm" => n.toInt >= 150 && n.toInt <= 193
      case heightPattern(n, unit) if unit == "in" => n.toInt >= 59 && n.toInt <= 76
      case _ => false
    }
  }

  def isValidHairColor(s: String): Boolean = {
    val hairPattern = "#([0-9]|[a-f]){6}".r
    hairPattern.matches(s)
  }

  def isValidEyeColor(s: String): Boolean = {
    val colors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    colors.contains(s)
  }

  def isValidPassport(s: String): Boolean = {
    val passportPattern = "[0-9]{9}".r
    passportPattern.matches(s)
  }

  def fromKeyValues(kvs: List[String]): Passport = {
    val valueMap = kvs.map(s => {
      val split = s.split(":")
      split(0) -> split(1)
    }).toMap

    Passport(
      birthYear = valueMap.get("byr").map(_.toInt),
      issueYear = valueMap.get("iyr").map(_.toInt),
      expirationYear = valueMap.get("eyr").map(_.toInt),
      height = valueMap.get("hgt"),
      hairColor = valueMap.get("hcl"),
      eyeColor = valueMap.get("ecl"),
      passportId = valueMap.get("pid"),
      countryId = valueMap.get("cid")
    )

  }


  def parseInput(input: String): List[Passport] = {
    input.split("\n\n").map(s => fromKeyValues(s.split("\\s+").toList)).toList
  }


  /**
   * read the problem's input and crash hard if it's invalid
   *
   * @param input the string of input coming from advent of code
   */
  override def validateInput(input: String): Unit = {
    parseInput(input).forall(_.isInstanceOf[Passport])
  }

  /**
   * solve part 1 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart1(input: String): String = {
    parseInput(input).count(_.isValid).toString
  }

  /**
   * solve part 2 of the day's problem
   *
   * @param input the string of input coming from advent of code
   * @return
   */
  override def solvePart2(input: String): String = {
    parseInput(input).count(_.isValid_withFields).toString
  }
}
