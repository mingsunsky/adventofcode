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
    val d1 = data.map(_.toInt)
    val d2: Stream[Int] = Stream.continually(d1.toStream).flatten
    val d3: Stream[(Int, Set[Int])] = d2.scanLeft((0, Set[Int]())){case((sum, list), e) => (sum + e, list + sum)}
    val d4 = d3.dropWhile(e => !e._2.contains(e._1))
    d4.head._1
  }

}