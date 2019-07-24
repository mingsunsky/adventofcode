package advent2018
import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

sealed trait Event { val value: String }
case object BeginShift extends Event { val value = "begins shift"}
case object FallSleep extends Event { val value = "falls asleep"}
case object WakeUp extends Event {val value = "wakes up"}

case class EventLog(date: LocalDateTime, id: Int, event: Event)



object Day4 extends Day4 with App {

  //Part1
  val data = Source.fromResource("Day1").getLines().toSeq
  println(part1(data))


  //Part2
//  println(part2(data))
}


trait Day4 {
  def part1(data: Seq[String]): Int = ???

  val pattern1 = "^\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\] Guard #([0-9]+) (.+)$".r
  val pattern2 = "^\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\] (.+)$".r
  def parseLine(line: String): EventLog = line match {
    case pattern1(y, m, d, h, mm, id, e) => EventLog(LocalDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt), id.toInt, matchEvent(e))
    case pattern2(y, m, d, h, mm, e) => EventLog(LocalDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt), 0, matchEvent(e))
  }

  private def matchEvent(event: String): Event = event match {
    case BeginShift.value => BeginShift
    case FallSleep.value => FallSleep
    case WakeUp.value => WakeUp
  }

}



