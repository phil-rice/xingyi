package org.validoc.utils.time

import org.validoc.utils.concurrency.Async

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


object NanoTimeService {
}

trait NanoTimeService {
  import Async._
  def apply(): Long
  def apply[M[_]: Async, Req, Res](delegate: Req => M[Res])(sideeffect: (Try[Res], Long) => Unit): Req => M[Res] = { req =>
    val startTime = apply()
    val result = delegate(req)
    result.registerSideEffectWhenComplete { res => sideeffect(res, apply() - startTime) }
    result
  }
}

object SystemClockNanoTimeService extends NanoTimeService {
  override def apply(): Long = System.nanoTime()
}



