package org.validoc.utils.time

import org.validoc.utils._
import org.validoc.utils.functions.MonadWithException

import scala.language.higherKinds
import scala.util.Try

trait NanoTimeService {
  def apply(): Long

}

object SystemClockNanoTimeService extends NanoTimeService {
  override def apply(): Long = System.nanoTime()
}



