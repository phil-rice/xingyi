package org.validoc.utils.logging

import org.validoc.utils.strings.Strings

import scala.util.Try

case class LoggingRecords(records: Vector[LoggingRecord] = Vector(), spans: Vector[Span] = Vector()) {
  def recordsJson = {
    lazy val startTime = records.map(_.time).min
    records.map(_.forJson(startTime))
  }

  def spanJson = {
    if (records.size == 0) Map()
    else {
      val startTime = records.minBy(_.time).time
      spans.map(_.forJson(startTime))
    }
  }

  def asciiArt(maxChars: Int): Vector[Map[String, String]] = {
    if (records.size == 0) Vector()
    else {
      val startTime = records.minBy(_.time).time
      val endTime = records.maxBy(_.time).time
      spans.map(_.asciiArt(startTime, maxChars, (endTime - startTime)))
    }
  }
}

case class Span(purpose: String, from: SpanId, startTime: Long, startMsg: String, midTime: Long, to: SpanId, endTime: Long, endMessage: String, result: String) {

  def forJson(initialTime: Long) =
    Map("purpose" -> purpose,
      "start" -> f"${(startTime - initialTime) / 1000000.0}%5.2f",
      "end" -> f"${(endTime - initialTime) / 1000000.0}%5.2f",
      "parseTime" -> f"${(endTime - midTime) / 1000000.0}%5.2f",
      "duration" -> f"${(endTime - startTime) / 1000000.0}%5.2f",
      "startMsg" -> startMsg,
      "endMsg" -> endMessage,
      "result" -> result)


  def asciiArt(initialTime: Long, maxChars: Int, maxDuration: Long) = {
    val scaledStart = (maxChars * (startTime - initialTime) / maxDuration).toInt
    val scaledDuration = Math.max(1, (maxChars * (endTime - startTime) / maxDuration).toInt)
    val line = (List.fill(scaledStart)(" ") ++ List.fill(scaledDuration)("*")).mkString("")
    forJson(initialTime) + ("line" -> line)
  }
}

case class SpanId(id: Int) extends AnyVal

case class LoggingReport[X](result: Try[X], records: LoggingRecords) {
  def recordsJson = records.recordsJson

  def spanJson = records.spanJson

  def asciiArt(maxChars: Int): Vector[Map[String, String]] = records.asciiArt(maxChars)
}


case class LoggingRecord(time: Long, level: String, msg: Any, throwable: Option[Throwable]) {
  def forJson(startTime: Long) = {
    val sinceStart = (time - startTime) / 1000000.0
    val rawMap = Map("time" -> f"$sinceStart%-3.2f", "level" -> level, "msg" -> msg, "summary" -> Strings.ellipses(500)(msg.toString))
    throwable.fold(rawMap)(t => rawMap + ("exception" -> t, "stackTrace" -> t.getStackTrace.mkString("\n")))
  }

}
