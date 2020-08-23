/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.metrics

import scala.language.higherKinds
import one.xingyi.core._
import one.xingyi.core.monad.MonadCanFailWithException
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

  def apply[Fail](prefix: String, duration: Long): (Try[Either[Fail, T]] => Map[String, MetricValue])
}

class DefaultReportData[T] extends ReportData[T] {
  def apply[Fail](prefix: String, duration: Long): (Try[Either[Fail, T]] => Map[String, MetricValue]) = {
    case Success(Right(t)) => succeeded(prefix, duration, t)
    case Success(Left(f)) => failed(prefix, duration, f)
    case Failure(t) => exception(prefix, duration, t)
  }
  def report(prefix: String, suffix: String, duration: Long)() =
    Map(prefix + "." + suffix -> CountMetricValue, prefix + "." + "duration" -> HistogramMetricValue(duration))

  def succeeded(prefix: String, duration: Long, t: T) = report(prefix, SuccessState.succeeded, duration)
  def failed[Fail](prefix: String, duration: Long, fail: Fail) = report(prefix, SuccessState.failed, duration)
  def exception(prefix: String, duration: Long, exception: Throwable) = report(prefix, SuccessState.exception, duration)
}

object ReportData {
  implicit def defaultReportData[Fail, T] = new DefaultReportData[T]
}

