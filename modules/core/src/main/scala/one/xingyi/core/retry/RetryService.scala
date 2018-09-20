/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.retry

import java.util.concurrent.atomic.AtomicLong

import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, MonadCanFailWithException}
import one.xingyi.core.time.Delay

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.{Success, Try}


trait RetryKleisli[M[_], Fail] {
  protected implicit def async: Async[M]
  protected implicit def monad: MonadCanFailWithException[M, Fail]

  def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Req => M[Res]): Req => M[Res] =
    new RetryService[M, Fail, Req, Res](raw, retryConfig)
}

trait NeedsRetry[T] {
  def apply[Fail](t: Try[Either[Fail, T]]): Boolean
}

object NeedsRetry {
  implicit def default[T] = new NeedsRetry[T] {
    override def apply[Fail](t: Try[Either[Fail, T]]): Boolean = t match {
      case Success(Right(_)) => false
      case _ => true
    }
  }
}

class RetryMetrics {
  private val timesRetried = new AtomicLong
  private val timesTotallyFailed = new AtomicLong

  def retry() = timesRetried.incrementAndGet()
  def failed() = timesTotallyFailed.incrementAndGet()

  def retryCount: Long = timesRetried.get()
  def failedCount: Long = timesTotallyFailed.get()

}


trait RetryInfo {
  def metrics: RetryMetrics
}

case class RetryConfig(retries: Int, delay: Delay)


class RetryService[M[_], Fail, Req, Res](val delegate: (Req => M[Res]), val retryConfig: RetryConfig)(implicit async: Async[M], monad: MonadCanFailWithException[M, Fail], resRetry: NeedsRetry[Res]) extends (Req => M[Res]) with RetryInfo {

  import retryConfig._

  val metrics = new RetryMetrics

  require(retries > 0, s"Retries should be more than 0 and are $retries")

  override def apply(req: Req): M[Res] = {
    def recurse(count: Int, retryThis: => M[Res]): M[Res] = {
      def retry(failM: M[Res]): M[Res] = {
        if (count == retries) metrics.retry
        if (count == 1) {
          metrics.failed
          failM
        } else {
          async.delay(delay())(recurse(count - 1, retryThis))
        }
      }
      withValue(retryThis)(m => m.mapTryFail[Fail, Res](tryRes => if (resRetry(tryRes)) retry(m) else m))
    }
    recurse(retries, delegate(req))
  }
}
