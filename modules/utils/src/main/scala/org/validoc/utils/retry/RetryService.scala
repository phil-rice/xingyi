package org.validoc.utils.retry

import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.concurrency.FuturableWithFailure
import org.validoc.utils.service.WrappingService
import org.validoc.utils.time.Delay

trait NeedsRetry[T] {
  def apply(t: T): Boolean
}

class RetryMetrics {
  private val timesRetried = new AtomicLong
  private val timesTotallyFailed = new AtomicLong

  def retry = timesRetried.incrementAndGet()

  def failed = timesTotallyFailed.incrementAndGet()
}

class RetryService[M[_], F, Req, Res](delegate: Req => M[Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay, retryFailed: Res => F)
                                     (implicit futurableWithFailure: FuturableWithFailure[M, F])
  extends WrappingService[M, Req, Res]("retry", delegate) {

  val metrics = new RetryMetrics

  import org.validoc.utils.concurrency.Futurable._

  require(retries > 0, s"Retries should be more than 0 and are $retries")

  override def apply(req: Req): M[Res] = {
    def recurse(count: Int): M[Res] =
      delegate(req).onComplete({ res: Res =>
        if (resRetry(res)) {
          if (count == 1) {
            metrics.failed
            futurableWithFailure.liftFailure[Res](retryFailed(res))
          }
          else {
            if (count == retries) metrics.retry
            futurableWithFailure.delay(delay()).flatMap(_ => recurse(count - 1))
          }
        }
        else futurableWithFailure.lift(res)
      }, { f: F => futurableWithFailure.liftFailure(f) })

    recurse(retries)
  }
}
