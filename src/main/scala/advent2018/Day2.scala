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

}

object Day2 extends Day2 with App {
  //Part1
  val data = Source.fromResource("Day2").getLines().toSeq
  println(part1(data))
}
