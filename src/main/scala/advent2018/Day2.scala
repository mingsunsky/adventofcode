package advent2018
import scala.io.Source

trait Day2 {

  def part1(lines: Seq[String]): Int = {
    val (a, b) = lines.map(find).unzip
    a.sum * b.sum
  }

  def find(letters: String): (Int, Int) = {
    val a: Set[Int] = letters.groupBy(identity)
      .values
      .map(_.length)
      .filter(_>1)
      .toSet

    (a.count(_ == 2), a.count(_ == 3))
  }

  def part2(lines: Seq[String]) = {
    val result= for {
      l1 <- lines
      l2 <- lines.takeRight(lines.indexOf(l1))
        if (numberOfDifferences(l1, l2) == 1)
    } yield (l1, l2)

    result
      .map { case (a, b) => commonLetters(a, b)}
      .head
  }

  def numberOfDifferences(s1: String, s2: String): Int = {
    s1.zip(s2)
      .count{case (char1, char2) => char1 != char2}
  }

  def commonLetters(s1: String, s2: String): String = {
    s1.zip(s2)
      .filter{case (a, b) => a == b}
      .map{ case (a, b) => a }
      .mkString
  }

}

object Day2 extends Day2 with App {
  //Part1
  val data = Source.fromResource("Day2").getLines().toSeq
  println(part1(data))

  //Part2
  println(part2(data))
}
