package org.validoc.utils.metrics

import java.util.concurrent.Executors

import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.concurrent.Eventually
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.concurrency.MDCPropagatingExecutionContext
import org.validoc.utils.time.MockTimeService

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.language.postfixOps

class MetricServiceTest extends FlatSpec with Matchers with MockitoSugar with Eventually {
  implicit val ec: MDCPropagatingExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  
  behavior of "Metrics"

  val exception = new RuntimeException

  val someMetrics = Map("someKey" -> CountMetricValue, "someOtherKey" -> HistogramMetricValue(100))

  def await[X](f: Future[X]) = Await.result(f, 5 seconds)

  def setup(fn: (MetricsService[Future, String, String], PutMetrics, ReportData[String]) => Unit): Unit = {
    implicit val timeService = new MockTimeService
    implicit val reportData = mock[ReportData[String]]
    implicit val putMetrics = mock[PutMetrics]

    when(putMetrics.apply(any[Map[String, MetricValue]])) thenReturn()

    val delegate: String => Future[String] = {
      case "fail" => Future.failed(exception)
      case s: String => Future(s + "_result")
    }

    fn(new MetricsService[Future, String, String]("prefix", delegate), putMetrics, reportData)
  }

  it should "record the metrics from a successful delegate call" in {
    setup { (metrics, putMetrics, reportData) =>
      when(reportData.apply("prefix", Success("succeed_result"), 100l)) thenReturn someMetrics
      await(metrics("succeed")) shouldBe "succeed_result"
      eventually(verify(putMetrics, times(1)).apply(someMetrics))
    }
  }

  it should "record the metrics from a  delegate call that throws an Exception" in {
    setup { (metrics, putMetrics, reportData) =>
      when(reportData.apply("prefix", Failure(exception), 100l)) thenReturn someMetrics
      intercept[RuntimeException](await(metrics("fail"))) shouldBe exception
      eventually(verify(putMetrics, times(1)).apply(someMetrics))
    }
  }
}


