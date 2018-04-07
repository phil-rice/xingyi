package one.xingyi.utils.time

import one.xingyi.utils._
import one.xingyi.utils.functions.MonadWithException

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



