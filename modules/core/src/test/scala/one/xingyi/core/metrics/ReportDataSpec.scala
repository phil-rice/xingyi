package one.xingyi.core.metrics

import one.xingyi.core.UtilsSpec

import scala.util.{Failure, Success}

class ReportDataSpec extends UtilsSpec {

  behavior of "Default Report Data"

  val rd = implicitly[ReportData[ String]]
  it should "pass the duration and a count of success when the result was successful" in {
    rd("prefix", 1000)(Success(Right("xxx"))) shouldBe Map("prefix.success" -> CountMetricValue, "prefix.duration" -> HistogramMetricValue(1000))
  }
  it should "pass the duration and a count of failed when the result was failed" in {
    rd("prefix", 1000)(Success(Left("xxx"))) shouldBe Map("prefix.failed" -> CountMetricValue, "prefix.duration" -> HistogramMetricValue(1000))
  }
  it should "pass the duration and a count of exception when the result was an exception" in {
    rd("prefix", 1000)(Failure(new RuntimeException)) shouldBe Map("prefix.exception" -> CountMetricValue, "prefix.duration" -> HistogramMetricValue(1000))
  }
}
