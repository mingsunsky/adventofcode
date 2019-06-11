package advent2018
import scala.io.Source

object Day1 extends Day1 with App {

  //Part1
  val data = Source.fromResource("Day1").getLines().toSeq
  println(part1(data))


  //Part2
  println(part2(data))
}


trait Day1 {
  def part1(data: Seq[String]): Int =  data.foldLeft(0)((sum, v) => sum + v.toInt)

  def part2(data: Seq[String]): Int = {
    Stream.continually(data.map(_.toInt).toStream).flatten
      .scanLeft((0, Set[Int]())){ case((sum, list), e) => (sum + e, list + sum) }
      .find { case(sum, list) => list.contains(sum) }
      .map{ case (sum, _) => sum }
      .get
  }

}