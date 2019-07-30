package advent2018

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.scalatest.{Matchers, WordSpec}
import Day4._

class Day4Spec extends WordSpec with Matchers {

  val datePattern = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  def parseDate(date: String): LocalDateTime = LocalDateTime.parse(date, datePattern)

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

  "part1 getShift" should {
    "get shifts for id 10" in {
      val events = """[1518-11-01 00:00] Guard #10 begins shift
                |[1518-11-01 00:05] falls asleep
                |[1518-11-01 00:25] wakes up
                |[1518-11-01 00:30] falls asleep
                |[1518-11-01 00:55] wakes up""".stripMargin.split("\n").map(parseLine)

      val expectedArr = ".....####################.....#########################.....".map {
        case '.' => 0
        case '#' => 1
      }.toArray

      val shift = getShift(events)
      shift.id shouldBe 10
      shift.sleeps shouldBe expectedArr
    }
  }

  "get shifts for id 99" in {
    val events = """[1518-11-01 23:58] Guard #99 begins shift
                |[1518-11-02 00:40] falls asleep
                |[1518-11-02 00:50] wakes up""".stripMargin.split("\n").map(parseLine)

    val expectedArr = "........................................##########..........".map {
      case '.' => 0
      case '#' => 1
    }.toArray

    val shift = getShift(events)
    shift.id shouldBe 99
    shift.sleeps shouldBe expectedArr
  }


  "part1 fixDate" should {
    "return same date when shift start at 00:00" in {
      val date = parseDate("1518-05-30 00:04")
      val log = EventLog(date, 10, BeginShift)
      fixEventDate(log) shouldBe log
    }

    "return next day at 00:00 when shift start at 23pm" in {
      val date = parseDate("1518-05-29 23:54")
      val log = EventLog(date, 10, BeginShift)
      val expectedLog = EventLog(parseDate("1518-05-30 00:00"), 10, BeginShift)
      fixEventDate(log) shouldBe expectedLog
    }

    "return same date when not shift start" in {
      val date = parseDate("1518-05-30 00:54")
      val log = EventLog(date, 10, WakeUp)
      fixEventDate(log) shouldBe log
    }
  }
}