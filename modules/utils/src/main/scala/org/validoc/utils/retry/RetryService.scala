package org.validoc.utils.retry

import java.util.concurrent.atomic.AtomicLong

import scala.util.Try

trait NeedsRetry[T] {
  def apply(t: Try[T]): Boolean
}

class RetryMetrics {
  private val timesRetried = new AtomicLong
  private val timesTotallyFailed = new AtomicLong

  def retry = timesRetried.incrementAndGet()

  def failed = timesTotallyFailed.incrementAndGet()
}

//class RetryService[M[_] : Async, F, Req, Res](delegate: Service[M, Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay, retryFailed: Res => F)
//  extends WrappingService[M, Req, Res]("retry", delegate) {
//
//  val metrics = new RetryMetrics
//
//  import org.validoc.utils.concurrency.Async._
//  import org.validoc.utils.monads.Monad._
//
//  require(retries > 0, s"Retries should be more than 0 and are $retries")
//
//  override def apply(req: Req): M[Res] = {
//    def recurse(count: Int): M[Res] =
//      delegate(req).onComplete({ res: Res =>
//        if (resRetry(res)) {
//          if (count == 1) {
//            metrics.failed
//            asyncWithFailure.liftFailure[Res](retryFailed(res))
//          }
//          else {
//            if (count == retries) metrics.retry
//            asyncWithFailure.delay(delay()).flatMap(_ => recurse(count - 1))
//          }
//        }
//        else asyncWithFailure.lift(res)
//      }, { f: F => asyncWithFailure.liftFailure(f) })
//
//    recurse(retries)
//  }
//}
