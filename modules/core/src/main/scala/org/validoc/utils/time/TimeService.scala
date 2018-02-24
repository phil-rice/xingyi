package org.validoc.utils.time

import org.validoc.utils.containers.Monad

import scala.language.higherKinds
import scala.util.Try

trait NanoTimeService {
  def apply(): Long
  def apply[M[_]: Monad, Req, Res](delegate: Req => M[Res])(sideeffect: (Try[Res], Long) => Unit): Req => M[Res] = { req =>
    val startTime = apply()
    val result = delegate(req)
//TODO    result.registerSideEffectWhenComplete { res => sideeffect(res, apply() - startTime) }
    result
  }
}

object SystemClockNanoTimeService extends NanoTimeService {
  override def apply(): Long = System.nanoTime()
}



