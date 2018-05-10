package one.xingyi.core.time

import one.xingyi.core._
import one.xingyi.core.functions.MonadWithException

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



