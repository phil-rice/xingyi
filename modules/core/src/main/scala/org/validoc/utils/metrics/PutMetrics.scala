package org.validoc.utils.metrics

import scala.language.higherKinds
import org.validoc.utils._
import org.validoc.utils.functions.MonadCanFailWithException
import org.validoc.utils.strings.Strings
import org.validoc.utils.success.SuccessState

import scala.util.{Failure, Success, Try}

trait PutMetrics extends (Map[String, MetricValue] => Unit)
object NullPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = ()
}
object PrintlnPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = println(v1.mkString("\n"))

}

trait MetricValue

object CountMetricValue extends MetricValue{
  override def toString = Strings.classNameOfObject(CountMetricValue)
}
case class HistogramMetricValue(name: Long) extends MetricValue

trait ReportData[Fail, T] extends ((String, Long) => Try[Either[Fail, T]] => Map[String, MetricValue]) {
  override def apply(prefix: String, duration: Long) = {
    case Success(Right(t)) => succeeded(prefix, duration, t)
    case Success(Left(f)) => failed(prefix, duration, f)
    case Failure(t) => exception(prefix, duration, t)
  }
  def succeeded(prefix: String, duration: Long, t: T): Map[String, MetricValue]
  def failed(prefix: String, duration: Long, fail: Fail): Map[String, MetricValue]
  def exception(prefix: String, duration: Long, exception: Throwable): Map[String, MetricValue]
}

object ReportData {
  implicit def defaultReportData[Fail, T] = new ReportData[Fail, T] {
    def report(prefix: String, suffix: String, duration: Long)() =
      Map(prefix + "." + suffix -> CountMetricValue, prefix + "." + "duration" -> HistogramMetricValue(duration))

    override def succeeded(prefix: String, duration: Long, t: T) = report(prefix, SuccessState.succeeded, duration)
    override def failed(prefix: String, duration: Long, fail: Fail) = report(prefix, SuccessState.failed, duration)
    override def exception(prefix: String, duration: Long, exception: Throwable) = report(prefix, SuccessState.exception, duration)
  }
}

