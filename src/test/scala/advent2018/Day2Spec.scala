package advent2018
import org.scalatest.{FlatSpec, Matchers}
import Day2.{find, numberOfDifferences}

class Day2Spec extends FlatSpec with Matchers {

  "part1" should "find repeating 2 and 3 times letters" in {
    find("abcdef") shouldEqual (0, 0)
    find("bababc") shouldEqual (1, 1)
    find("abbcde") shouldEqual (1, 0)
    find("abcccd") shouldEqual (0, 1)
    find("aabcdd") shouldEqual (1, 0)
    find("abcdee") shouldEqual (1, 0)
    find("ababab") shouldEqual (0, 1)
  }

  "part2" should "find number of different chars" in {
    numberOfDifferences("abcde", "axcye") shouldEqual 2
    numberOfDifferences("fghij", "fguij") shouldEqual 1
  }

//  "part1" should "pass read data test" in {
//    import scala.io.Source
//    val data = Source.fromResource("Day2").getLines().toSeq
//    part1(data) shouldEqual 4693
//  }
}
