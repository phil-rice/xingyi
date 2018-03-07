package org.validoc.utils.metrics

import java.util.concurrent.Executors

import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.concurrent.Eventually
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.UtilsSpec
import org.validoc.utils.functions.ScalaFutureAsAsyncAndMonadAndFailer
import org.validoc.utils.time.MockTimeService

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

class MetricServiceTest extends UtilsSpec {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  behavior of "Metrics"

  val exception = new RuntimeException

  val someMetrics = Map("someKey" -> CountMetricValue, "someOtherKey" -> HistogramMetricValue(100))


  def setup(fn: (String => Future[String], (String => Future[String]), PutMetrics) => Unit): Unit = {
    implicit val timeService = new MockTimeService
    implicit val reportData = new DefaultReportData[Throwable, String] {
      override def apply(prefix: String, duration: Long) = {
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


