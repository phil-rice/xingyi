package org.validoc.utils.time

import scala.concurrent.duration._
import scala.util.Random
import scala.language.postfixOps
trait Delay {
  def apply(): FiniteDuration
}


object RandomDelay {
  val random = new Random()
}

class RandomDelay(duration: FiniteDuration) extends Delay {
  override def apply(): FiniteDuration = {
    ((0.5 + RandomDelay.random.nextDouble) * duration.toMicros).toLong micros
  }
}