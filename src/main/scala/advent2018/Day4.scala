package advent2018
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import scala.io.Source

sealed trait Event { val value: String }
case object BeginShift extends Event { val value = "begins shift"}
case object FallSleep extends Event { val value = "falls asleep"}
case object WakeUp extends Event {val value = "wakes up"}

case class EventLog (date: LocalDateTime, id: Option[Int], event: Event)

case class Shift (id: Option[Int], sleeps: Array[Int]) {
  override def toString: String = s"$id " + sleeps.mkString
}

object Day4Solution extends Day4 with App {

  //Part1
  val data = Source.fromResource("Day4").getLines().toSeq
  println(part1(data))

  //Part2
  println(part2(data))
}


trait Day4 {
  def part1(data: Seq[String]): Int = {

    val shifts = getShiftMapWithId(data)
    val (guardId, _) = shifts.mapValues(_.sleeps.sum).maxBy(_._2)
    val sleeps = shifts(guardId).sleeps
    val maxMinute = sleeps.max
    val minute = sleeps.indexWhere(_ == maxMinute)
    guardId.fold(0)(_ * minute)
  }

  def part2(data: Seq[String]): Int = {
    val shifts = getShiftMapWithId(data)
    val guardId = shifts.mapValues(_.sleeps.max).maxBy(_._2)._1
    val sleeps = shifts(guardId).sleeps
    val maxMinute = sleeps.max
    val minute = sleeps.indexWhere(_ == maxMinute)
    guardId.fold(0)(_ * minute)
  }

  def getShiftMapWithId(data: Seq[String]): Map[Option[Int], Shift] = {
    import cats.Monoid
    import cats.implicits._

    implicit val shiftMonoid: Monoid[Shift] = new Monoid[Shift] {
      override def empty: Shift = Shift(None, new Array[Int](60))
      override def combine(x: Shift, y: Shift): Shift = Shift(None, x.sleeps.zip(y.sleeps).map {case (a, b) => a + b})
    }

    val shiftsWithDate: Map[LocalDate, Shift] = data.map(parseLine)
      .map(fixEventDate)
      .sorted(Ordering.fromLessThan[EventLog]((a, b) => a.date isBefore b.date))
      .groupBy(_.date.toLocalDate)
      .mapValues(getShift _)

    shiftsWithDate
      .values
      .groupBy(_.id)
      .mapValues(_.toList.combineAll)
  }

  val pattern1 = """^\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] Guard #(\d{1,4}) (.+)$""".r
  val pattern2 = """^\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (.+)$""".r
  val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  def parseLine(line: String): EventLog = line match {
    case pattern1(d, id, e) => EventLog(LocalDateTime.parse(d, dateFormat), Some(id.toInt), matchEvent(e))
    case pattern2(d, e) => EventLog(LocalDateTime.parse(d, dateFormat), None, matchEvent(e))
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

    Shift(log.find(_.event == BeginShift).flatMap(_.id), array)
  }
}



