package advent2018
import java.time.{LocalDate, LocalDateTime}

import com.sun.net.httpserver.Authenticator.Failure

import scala.io.Source

sealed trait Event { val value: String }
case object BeginShift extends Event { val value = "begins shift"}
case object FallSleep extends Event { val value = "falls asleep"}
case object WakeUp extends Event {val value = "wakes up"}

case class EventLog (date: LocalDateTime, id: Int, event: Event)

case class Shift (id: Int, sleeps: Array[Int])

object Day4 extends Day4 with App {

  //Part1
  val data = Source.fromResource("Day1").getLines().toSeq
  println(part1(data))


  //Part2
//  println(part2(data))
}


trait Day4 {
  def part1(data: Seq[String]): Int = {
    import cats.Monoid
    import cats.implicits._

    implicit val shiftMonoid: Monoid[Shift] = new Monoid[Shift] {
      override def empty: Shift = Shift(0, new Array[Int](60))
      override def combine(x: Shift, y: Shift): Shift = Shift(x.id, x.sleeps.zip(y.sleeps).map {case (a, b) => a + b})
    }

    val log: Map[Int, Iterable[Shift]] = data.map(parseLine)
      .map(fixEventDate)
      .sortWith {case (a, b) => a.date.compareTo(b.date) < 0 }
      .groupBy(_.date.toLocalDate)
      .mapValues(getShift _)
      .values
      .groupBy(_.id)

    val shifts: Map[Int, Shift] = log.mapValues(_.combineAll)

    shifts.mapValues(_.sleeps.sum)
      .max
      ._1
  }

//    [1518-05-30 00:04] Guard #2417 begins shift
//    [1518-10-20 00:48] wakes up

  val pattern1 = """^\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] Guard #(\d{1,4}) (.+)$""".r
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


  def fixEventDate(log: EventLog): EventLog = log match {
    case EventLog(dt, id, BeginShift) if (dt.getHour == 23) => EventLog(dt.plusDays(1).withHour(0).withMinute(0), id, BeginShift)
    case _ => log
  }

  def getShift(log: Seq[EventLog]): Shift = {
    val array = log.foldLeft(new Array[Int](60))((acc, l) => l.event match {
      case BeginShift => acc
      case FallSleep => acc.slice(0, l.date.getMinute) ++ acc.slice(l.date.getMinute, 60).map(_ => 1)
      case WakeUp => acc.slice(0, l.date.getMinute) ++ acc.slice(l.date.getMinute, 60).map(_ => 0)
    })

    Shift(log.find(_.event == BeginShift).map(_.id).getOrElse(0), array)
  }
}



