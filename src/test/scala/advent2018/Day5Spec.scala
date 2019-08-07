package advent2018

import org.scalatest.{Matchers, WordSpec}
import Day5Solution._

class Day5Spec extends WordSpec with Matchers {

  "part1 react" should {
    "return correct reactions" in {
      react("aA") shouldBe ""
      react("abBA") shouldBe ""
      react("abAB") shouldBe "abAB"
      react("aabAAB") shouldBe "aabAAB"
      react("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"
    }
  }

  "part2 remove" should {
    "return correct reactions" in {
      remove("dabAcCaCBAcCcaDA", 'a') shouldBe "dbcCCBcCcD"
      remove("dabAcCaCBAcCcaDA", 'b') shouldBe "daAcCaCAcCcaDA"
      remove("dabAcCaCBAcCcaDA", 'c') shouldBe "dabAaBAaDA"
      remove("dabAcCaCBAcCcaDA", 'd') shouldBe "abAcCaCBAcCcaA"
    }
  }

}
