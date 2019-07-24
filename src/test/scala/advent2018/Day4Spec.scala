package advent2018

import java.time.LocalDateTime

import org.scalatest.{Matchers, WordSpec}
import Day4.parseLine

class Day4Spec extends WordSpec with Matchers {

  "part1 parseLine" should {
    "parse begins shift" in {
      val result = parseLine("[1518-05-30 00:04] Guard #2417 begins shift")
      result.date shouldEqual LocalDateTime.of(1518, 5, 30, 0, 4)
      result.id shouldEqual 2417
      result.event shouldEqual BeginShift
    }

    "parse wakes up" in {
      val result = parseLine("[1518-10-20 00:48] wakes up")
      result.date shouldEqual LocalDateTime.of(1518, 10, 20, 0, 48)
      result.id shouldEqual 0
      result.event shouldEqual WakeUp
    }

    "parse falls sleep" in {
      val result = parseLine("[1518-08-12 00:14] falls asleep")
      result.date shouldEqual LocalDateTime.of(1518, 8, 12, 0, 14)
      result.id shouldEqual 0
      result.event shouldEqual FallSleep
    }
  }
}