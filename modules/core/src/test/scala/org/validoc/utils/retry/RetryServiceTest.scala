package org.validoc.utils.retry

import java.util.concurrent.Executors

import org.validoc.utils.time.Delay
import org.validoc.utils.{Service, UtilsWithLoggingSpec}

import scala.concurrent.{ExecutionContext, Future}
import org.mockito.Mockito._
import org.validoc.utils.concurrency.MDCPropagatingExecutionContext

import scala.concurrent.duration._
import scala.util.Success

class RetryServiceTest extends UtilsWithLoggingSpec {
  type Req = String
  type Res = String

  behavior of "RetryService"

  def setup(fn: (RetryService[Future, Req, Res], Service[Future, Req, Res], NeedsRetry[Res], Delay) => Unit) = {
    val retry = mock[NeedsRetry[Res]]
    val delay = mock[Delay]
    when(delay.apply()) thenReturn (1 milli)
    val delegate = mock[Service[Future, Req, Res]]
    fn(new RetryService[Future, Req, Res](delegate, retry, 2, delay), delegate, retry, delay)
  }


  it should "just call the delegate once if it returns and the retry strategy says 'it's OK' " in {
    setup { (retryService, delegate, needsRetry, delay) =>
      when(delegate("req1")) thenReturn Future.successful("result1")
      when(needsRetry.apply(Success("result1"))) thenReturn false
      await(retryService("req1")) shouldBe "result1"
      retryService.metrics.retryCount shouldBe 0
      retryService.metrics.failedCount shouldBe 0
      verify(delay, times(0)).apply
      verify(delegate, times(1)).apply("req1")
    }
  }
  it should "just call the delegate twice if it returns and the retry strategy says 'retry' followed by 'OK' " in {
    setup { (retryService, delegate, needsRetry, delay) =>
      when(delegate("req1")) thenReturn(Future.successful("result1"), Future.successful("result2"))
      when(needsRetry.apply(Success("result1"))) thenReturn true
      when(needsRetry.apply(Success("result2"))) thenReturn false
      await(retryService("req1")) shouldBe "result2"
      retryService.metrics.retryCount shouldBe 1
      retryService.metrics.failedCount shouldBe 0
      verify(delay, times(1)).apply
      verify(delegate, times(2)).apply("req1")

    }
  }

  it should "call the delegate twice and fail with the final result if the retry strategy says 'retry' and the count is up" in {
    setup { (retryService, delegate, needsRetry, delay) =>
      when(delegate("req1")) thenReturn(Future.successful("result1"), Future.successful("result2"))
      when(needsRetry.apply(Success("result1"))) thenReturn true
      when(needsRetry.apply(Success("result2"))) thenReturn true

      await(retryService("req1")) shouldBe "result2"
      retryService.metrics.retryCount shouldBe 1
      verify(delegate, times(2)).apply("req1")
      retryService.metrics.failedCount shouldBe 1
      verify(delay, times(1)).apply
    }
  }
}