package advent2018

import org.scalatest.{FlatSpec, Matchers}
import Day3.{parseLine, fillArray}

class Day3Spec extends FlatSpec with Matchers {

  "part1" should "parse line to Claim object" in {
      parseLine("#1 @ 527,351: 24x10") shouldEqual(Claim(1, 527, 351, 24, 10))
  }

  "part1" should "fill single fabric claim" in {
    val fill = fillArray(Array.ofDim[Int](6, 8), Claim(1, 3, 2, 5, 4))
    val expected = Array(
      Array(0,0,0,0,0,0,0,0),
      Array(0,0,0,0,0,0,0,0),
      Array(0,0,0,1,1,1,1,1),
      Array(0,0,0,1,1,1,1,1),
      Array(0,0,0,1,1,1,1,1),
      Array(0,0,0,1,1,1,1,1),
    )

    fill shouldEqual expected
  }

  "part1" should "fill multiple fabric claims" in {
    val fill1 = fillArray(Array.ofDim[Int](8, 8), Claim(1, 1, 3, 4, 4))
    val fill2 = fillArray(fill1, Claim(2, 3, 1, 4, 4))
    val fill3 = fillArray(fill2, Claim(3, 5, 5, 2, 2))
    val expected = Array(
      Array(0,0,0,0,0,0,0,0),
      Array(0,0,0,1,1,1,1,0),
      Array(0,0,0,1,1,1,1,0),
      Array(0,1,1,2,2,1,1,0),
      Array(0,1,1,2,2,1,1,0),
      Array(0,1,1,1,1,1,1,0),
      Array(0,1,1,1,1,1,1,0),
      Array(0,0,0,0,0,0,0,0),
    )

    fill3 shouldEqual expected
  }


}
