package advent2018

import java.io

import scala.io.Source

object Day3 extends App with Day3 {
  //Part1
  val data = Source.fromResource("Day3").getLines().toSeq
  println(part1(data))

  //Part2
  println(part2(data))

}


trait Day3 {
  val pattern = "^#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)$".r

  def parseLine(line: String): Claim = line match {
      case pattern(id, x, y, w, h) => Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
  }

  def fillArray(fabric: Array[Array[Int]], claim: Claim): Array[Array[Int]] = {
    claim.toPixels.foreach { case (x, y) => fabric(y)(x) += 1 }
    fabric
  }

  def part1(data: Seq[String]): Int = {
    data.map(parseLine)
      .foldLeft(Array.ofDim[Int](1000, 1000))(fillArray)
      .flatten.count(_>1)
  }

  def part2(data: Seq[String]): Int = {
    val claims = data.map(parseLine)
    val fabric = claims.foldLeft(Array.ofDim[Int](1000, 1000))(fillArray)
    claims.find(
      _.toPixels
      .count{ case (x, y) => fabric(y)(x) > 1} == 0
    ).get.id
  }

}

case class Claim(id: Int, posX: Int, posY: Int, wide: Int, height: Int) {
  def toPixels: Seq[(Int, Int)] = for {
    x <- posX to posX + wide -1
    y <- posY to posY + height -1
  } yield (x, y)
}