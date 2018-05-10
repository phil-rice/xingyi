package one.xingyi.core.metrics

import scala.language.higherKinds
import one.xingyi.core._
import one.xingyi.core.functions.MonadCanFailWithException
import one.xingyi.core.strings.Strings
import one.xingyi.core.success.SuccessState

import scala.util.{Failure, Success, Try}

trait PutMetrics extends (Map[String, MetricValue] => Unit)

object NullPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = ()
}

object PrintlnPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = println(v1.mkString("\n"))

}

trait MetricValue

object CountMetricValue extends MetricValue {
  override def toString = Strings.classNameOfObject(CountMetricValue)
}

case class HistogramMetricValue(name: Long) extends MetricValue

trait ReportData[T] {

  def apply[Fail](prefix: String, duration: Long): (Try[Either[Fail, T]] => Map[String, MetricValue]) = {
    case Success(Right(t)) => succeeded(prefix, duration, t)
    case Success(Left(f)) => failed(prefix, duration, f)
    case Failure(t) => exception(prefix, duration, t)
  }
  def succeeded(prefix: String, duration: Long, t: T): Map[String, MetricValue]
  def failed[Fail](prefix: String, duration: Long, fail: Fail): Map[String, MetricValue]
  def exception(prefix: String, duration: Long, exception: Throwable): Map[String, MetricValue]
}

class DefaultReportData[ T] extends ReportData[ T] {
  def report(prefix: String, suffix: String, duration: Long)() =
    Map(prefix + "." + suffix -> CountMetricValue, prefix + "." + "duration" -> HistogramMetricValue(duration))

  override def succeeded(prefix: String, duration: Long, t: T) = report(prefix, SuccessState.succeeded, duration)
  override def failed[Fail](prefix: String, duration: Long, fail: Fail) = report(prefix, SuccessState.failed, duration)
  override def exception(prefix: String, duration: Long, exception: Throwable) = report(prefix, SuccessState.exception, duration)
}

object ReportData {
  implicit def defaultReportData[Fail, T] = new DefaultReportData[ T]
}

