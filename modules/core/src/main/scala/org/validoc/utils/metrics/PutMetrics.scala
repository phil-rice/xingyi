package org.validoc.utils.metrics

import org.validoc.utils.success.SucceededState

import scala.util.Try

sealed trait MetricValue

object CountMetricValue extends MetricValue

case class HistogramMetricValue(name: Long) extends MetricValue

trait ReportData[Fail, T] extends (String => (Long, SucceededState[Fail, T]) => Map[String, MetricValue])

object ReportData {
  implicit def defaultReportData[Fail, T] = new ReportData[Fail, T] {
    override def apply(prefix: String) = { ( duration,state) =>
      Map(prefix + "." + state.asKey -> CountMetricValue,
        prefix + "." + "duration" -> HistogramMetricValue(duration))
    }
  }
}

