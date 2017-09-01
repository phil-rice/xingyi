package org.validoc.utils.retry

import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.service._
import org.validoc.utils.time.Delay

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Try

trait NeedsRetry[T] {
  def apply(t: Try[T]): Boolean
}

object NeedsRetry {
  implicit def default[T] = new NeedsRetry[T] {
    override def apply(t: Try[T]): Boolean = t.isFailure
  }
}

class RetryMetrics {
  private val timesRetried = new AtomicLong
  private val timesTotallyFailed = new AtomicLong

  def retry = timesRetried.incrementAndGet()

  def failed = timesTotallyFailed.incrementAndGet()

  def retryCount = timesRetried.get()

  def failedCount = timesTotallyFailed.get()

}

trait RetryInfo {
  def metrics: RetryMetrics
}

case class RetryConfig(retries: Int, delay: Delay)



class RetryService[M[_] : Async, Req, Res](delegate: Service[M, Req, Res], retryConfig: RetryConfig)(implicit resRetry: NeedsRetry[Res]) extends Service[M, Req, Res] with RetryInfo {

  import retryConfig._

  val metrics = new RetryMetrics
  val async = implicitly[Async[M]]

  import Async._

  require(retries > 0, s"Retries should be more than 0 and are $retries")

  override def apply(req: Req): M[Res] = {
    def recurse(count: Int): M[Res] =
      delegate(req).transform(tryReq =>
        if (resRetry(tryReq)) {
          if (count == 1) {
            metrics.failed
            async.liftTry(tryReq)
          } else {
            if (count == retries) metrics.retry
            async.delay(delay()).flatMap(_ => recurse(count - 1))
          }
        }
        else async.liftTry(tryReq))

    recurse(retries)
  }
}
