package advent2018

import scala.io.Source

object Day5Solution extends App with Day5 {

  val data = Source.fromResource("Day5").getLines().mkString

  //part1
  println(part1(data))

  //part2
  println(part2(data))
}


trait Day5 {

  def react(value: String): String = {
    value.foldLeft(" ")((a, b) => if (Math.abs(a.last - b)==32) a.init else a + b).tail
  }

  def remove(value: String, char: Char) = {
    value.replaceAll(char.toString.toLowerCase(), "")
      .replaceAll(char.toString.toUpperCase(), "")
  }

  def part1(data: String): Int = react(data).length

  def part2(data: String): Int = {
    ('a' to 'z').map(remove(data, _))
      .map(react)
      .map(_.length)
      .min
  }

}