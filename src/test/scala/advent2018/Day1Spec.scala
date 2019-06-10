package advent2018
import org.scalatest.{FlatSpec, Matchers}
import Day1.{part1, part2}

class Day1Spec extends FlatSpec with Matchers {

  "part1" should "return final frequency" in {
    part1(List("+1", "+1", "+1")) shouldEqual 3
    part1(List("+1", "+1", "-2")) shouldEqual 0
    part1(List("-1", "-2", "-3")) shouldEqual -6
  }

  "part2" should "return first duplicates" in {
    part2(List("+1", "-1")) shouldEqual 0
    part2(List("+3", "+3", "+4", "-2", "-4")) shouldEqual 10
    part2(List("-6", "+3", "+8", "+5", "-6")) shouldEqual 5
    part2(List("+7", "+7", "-2", "-7", "-4")) shouldEqual 14
  }

}
