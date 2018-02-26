package org.validoc.utils.retry

import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.Service
import org.validoc.utils.time.Delay

import scala.language.higherKinds
import scala.util.{Success, Try}
import org.validoc.utils._
import org.validoc.utils.concurrency.Async
import org.validoc.utils.functions.{Functions, Monad, MonadCanFail}

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


class RetryService[M[_], Fail, Req, Res](delegate: Service[M, Req, Res], retryConfig: RetryConfig)(implicit async: Async[M], monad: MonadCanFail[M, Fail], resRetry: NeedsRetry[Fail, Res]) extends Service[M, Req, Res] with RetryInfo {

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
