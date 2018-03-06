package org.validoc.utils.retry

import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils._
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.time.Delay

import scala.language.higherKinds
import scala.util.{Success, Try}
import org.validoc.utils.language.Language._

import scala.reflect.ClassTag


trait RetryKleisli[M[_], Fail] {
  protected implicit def async: Async[M]
  protected implicit def monad: MonadCanFailWithException[M, Fail]

  def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: Req => M[Res])(implicit retry: NeedsRetry[Fail, Res]): Req => M[Res] =
    new RetryService[M, Fail, Req, Res](raw, retryConfig)
}

trait NeedsRetry[Fail, T] {
  def apply(t: Try[Either[Fail, T]]): Boolean
}

object NeedsRetry {
  implicit def default[Fail, T] = new NeedsRetry[Fail, T] {
    override def apply(t: Try[Either[Fail, T]]): Boolean = t match {
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


class RetryService[M[_], Fail, Req, Res](delegate: (Req => M[Res]), retryConfig: RetryConfig)(implicit async: Async[M], monad: MonadCanFailWithException[M, Fail], resRetry: NeedsRetry[Fail, Res]) extends (Req => M[Res]) with RetryInfo {

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
      withValue(retryThis)(m => monad.mapTryFail[Res, Res](m, tryRes => if (resRetry(tryRes)) retry(m) else m))
    }
    recurse(retries, delegate(req))
  }
}
