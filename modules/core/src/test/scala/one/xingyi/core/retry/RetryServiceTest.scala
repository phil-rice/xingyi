/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.retry

import one.xingyi.core.UtilsWithLoggingSpec
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer
import one.xingyi.core.time.Delay
import org.mockito.Mockito._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success

class RetryServiceTest extends UtilsWithLoggingSpec with ScalaFutureAsAsyncAndMonadAndFailer with RetryKleisli[Future, Throwable] {
  type Req = String
  type Res = String

  behavior of "RetryService"

  def setup(fn: (RetryService[Future, Throwable, Req, Res], (Req => Future[Res]), NeedsRetry[Res], Delay) => Unit) = {
    implicit val needsRetry = mock[NeedsRetry[Res]]
    val delay = mock[Delay]
    when(delay.apply()) thenReturn (1 milli)
    val delegate = mock[(Req => Future[Res])]
    fn(retry(RetryConfig(2, delay))(delegate).asInstanceOf[RetryService[Future, Throwable, Req, Res]], delegate, needsRetry, delay)
  }

  it should "blowUp if retries are 0" in {
    val delay = mock[Delay]
    when(delay.apply()) thenReturn (1 milli)
    val delegate = mock[(Req => Future[Res])]
    intercept[IllegalArgumentException](new RetryService[Future, Throwable, Req, Res](delegate, RetryConfig(0, delay)))
  }

  it should "just call the delegate once if it returns and the retry strategy says 'it's OK' " in {
    setup { (retryService, delegate, needsRetry, delay) =>
      when(delegate("req1")) thenReturn Future.successful("result1")
      when(needsRetry.apply(Success(Right("result1")))) thenReturn false
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
      when(needsRetry.apply(Success(Right("result1")))) thenReturn true
      when(needsRetry.apply(Success(Right("result2")))) thenReturn false


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
      when(needsRetry.apply(Success(Right("result1")))) thenReturn true
      when(needsRetry.apply(Success(Right("result2")))) thenReturn true

      await(retryService("req1")) shouldBe "result2"
      retryService.metrics.retryCount shouldBe 1
      verify(delegate, times(2)).apply("req1")
      retryService.metrics.failedCount shouldBe 1
      verify(delay, times(1)).apply
    }
  }
}
