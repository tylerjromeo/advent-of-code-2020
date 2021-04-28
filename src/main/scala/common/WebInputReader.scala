package org.romeo.adventofcode
package common
import sttp.client3._

object WebInputReader {

  def url(day: Int) = uri"https://adventofcode.com/2020/day/$day/input"

  val cookie = "fill in cookie"

  private val sttpBackend = HttpURLConnectionBackend()
  def getInput(day: Int): Either[String, String] = {
    basicRequest
      .header("cookie", cookie)
      .get(url(day))
      .send(sttpBackend).body
  }
}
