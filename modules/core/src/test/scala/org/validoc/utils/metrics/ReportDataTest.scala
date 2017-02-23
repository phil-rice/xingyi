package org.validoc.utils.metrics

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class ReportDataTest extends FlatSpec with Matchers {

  behavior of "DefaultReportData"

  def getData(tryT: Try[Either[Int, String]]) = implicitly[ReportData[Either[Int , String]]].apply("somePrefix", tryT, 100l)

  it should "turn a Try[error\\/data]' into a map, when left is a failure and right is a success" in {
    getData(Success(Right("value"))) shouldBe Map("somePrefix.success" -> CountMetricValue, "somePrefix.duration" -> HistogramMetricValue(100))
    getData(Success(Left(123))) shouldBe Map("somePrefix.failure" -> CountMetricValue, "somePrefix.duration" -> HistogramMetricValue(100))
    getData(Failure(new RuntimeException)) shouldBe Map("somePrefix.exception" -> CountMetricValue, "somePrefix.duration" -> HistogramMetricValue(100))
  }
}
