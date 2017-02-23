package org.validoc.utils.logging

import org.validoc.utils.UtilsSpec

import scala.util.Success


class LoggingRecordTests extends UtilsSpec {


  private val runtimeException: RuntimeException = new RuntimeException("someMessage")

  def toMs(x: Int) = x * 1000000

  val record1 = LoggingRecord(time = toMs(5), level = Debug, "someMsg", None)
  val record2 = LoggingRecord(time = toMs(10), level = Debug, "someMsg", Some(runtimeException))

  "LoggingRecord.asJson" should "return a map when there is no exception, subtracting the startTime from the time" in {
    record1.forJson(0) shouldBe
      Map("time" -> "5.00", "level" -> Debug, "msg" -> "someMsg", "summary" -> "someMsg")
    record1.forJson(toMs(0))("time") shouldBe "5.00"
    record1.forJson(toMs(4))("time") shouldBe "1.00"
  }

  it should "return a map when there is a exception" in {
    val actual = record2.forJson(0)
    val expected = Map("exception" -> runtimeException, "stackTrace" -> runtimeException.getStackTrace.mkString("\n"),
      "msg" -> "someMsg", "time" -> "10.00", "summary" -> "someMsg", "level" -> Debug)
    compareSequences(expected.toSeq, actual.toSeq)
  }

  "LoggingReport.asJson" should "return the aggregate of the Logging Records" in {
    LoggingReport(Success("someResult"), LoggingRecords(Vector(record1, record2))).recordsJson shouldBe Vector(record1.forJson(toMs(5)), record2.forJson(toMs(5)))
  }

}
