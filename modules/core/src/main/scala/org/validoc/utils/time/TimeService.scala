package org.validoc.utils.time

import org.validoc.utils._
import org.validoc.utils.functions.MonadWithException

import scala.language.higherKinds
import scala.util.Try

trait NanoTimeService {
  def apply(): Long

}

object NanoTimeService {

  implicit object DefaultNanotimeService extends NanoTimeService {
    override def apply() = System.nanoTime()
  }
}



