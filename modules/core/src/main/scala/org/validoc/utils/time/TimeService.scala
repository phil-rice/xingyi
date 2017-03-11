package org.validoc.utils.time

import org.validoc.utils.concurrency.Async
import org.validoc.utils.service.ServerContext

import scala.language.higherKinds
import scala.util.Try


object NanoTimeService {
  implicit def timeServiceIfTheresAServiceContextInScope(implicit serverContext: ServerContext[_,_]) = serverContext.timeService
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



