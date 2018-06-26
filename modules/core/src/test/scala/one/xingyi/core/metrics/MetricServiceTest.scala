/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.metrics

import java.util.concurrent.Executors

import one.xingyi.core.UtilsSpec
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer
import one.xingyi.core.time.MockTimeService
import org.mockito.Mockito._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

class MetricServiceTest extends UtilsSpec {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  behavior of "Metrics"

  val exception = new RuntimeException

  val someMetrics = Map("someKey" -> CountMetricValue, "someOtherKey" -> HistogramMetricValue(100))


  def setup(fn: (String => Future[String], (String => Future[String]), PutMetrics) => Unit): Unit = {
    implicit val timeService = new MockTimeService
    implicit val reportData = new DefaultReportData[ String] {
      override def apply[Fail](prefix: String, duration: Long) = {
        case Success(t) => Map("markerS" -> CountMetricValue)
        case Failure(t) => Map("markerF" -> CountMetricValue)
      }
    }
    val mockPutMetrics = mock[PutMetrics]
    val delegate = mock[String => Future[String]]
    val metricsKleisli = new MetricsKleisli[Future, Throwable] with ScalaFutureAsAsyncAndMonadAndFailer {
      override protected def putMetrics = mockPutMetrics
      override protected def timeService = new MockTimeService
    }
    fn(metricsKleisli.metrics("metricsPrefix")(delegate), delegate, mockPutMetrics)
  }

  it should "return the result of the delegate, sending the result of the reportData to the putMetrics when successful" in {
    setup { (metrics, delegate, putMetrics) =>
      when(delegate.apply("input")) thenReturn Future.successful("output")
      await(metrics("input")) shouldBe "output"
     eventually( verify(putMetrics, times(1)).apply(Map("markerS" -> CountMetricValue)))
    }
  }

  it should "record the metrics from a  delegate call that throws an Exception" in {
    setup { (metrics, delegate, putMetrics) =>
      when(delegate.apply("input")) thenReturn Future.failed(exception)
      val m = metrics("input")
      intercept[Exception](await(m)) shouldBe exception

      eventually(verify(putMetrics, times(1)).apply(Map("markerF" -> CountMetricValue)))
    }
  }
}


