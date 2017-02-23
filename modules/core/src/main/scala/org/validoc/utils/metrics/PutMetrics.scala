package org.validoc.utils.metrics

import scala.util.{Failure, Success, Try}

sealed trait MetricValue

object CountMetricValue extends MetricValue

case class HistogramMetricValue(name: Long) extends MetricValue


trait ReportData[T] extends ((String, Try[T], Long) => Map[String, MetricValue]) {
}

object ReportData {
  implicit def defaultReportData[T](implicit succeeded: Succeeded[T]) = new ReportData[T] {
    def apply(prefix: String, tryT: Try[T], duration: Long): Map[String, MetricValue] =
      Map(prefix + "." + succeeded(tryT).asKey -> CountMetricValue,
        prefix + "." + "duration" -> HistogramMetricValue(duration))
  }
}

