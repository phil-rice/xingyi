package org.validoc.utils.metrics

import org.validoc.utils.success.{ExceptionState, FailedState, Succeeded, SuccessState}

import scala.util.{Failure, Success, Try}

sealed trait MetricValue


object CountMetricValue extends MetricValue

case class HistogramMetricValue(name: Long) extends MetricValue

trait ReportData[T] {
  def reportData(t: Try[T], duration: Long): Map[String, MetricValue]
}

object ReportData {
  implicit def reportDataForTry[T](implicit succeeded: Succeeded[T]) = new ReportData[T] {
    override def reportData(t: Try[T], duration: Long): Map[String, MetricValue] =
      Map(succeeded(t).asKey -> CountMetricValue, "duration" -> HistogramMetricValue(duration))
  }
}