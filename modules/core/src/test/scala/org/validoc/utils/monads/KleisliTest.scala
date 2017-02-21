package org.validoc.utils.monads

import org.validoc.utils.UtilsSpec
import org.validoc.utils.concurrency.Async

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

abstract class AbstractKleisliTest[M[_] : Async] extends UtilsSpec {
  behavior of "Kleisli"

  import org.validoc.utils.concurrency.Async._
  import Kleisli._

  val first: Int => M[String] = _.toString.liftValue[M]
  val second: String => M[Double] = _.toDouble.liftValue[M]

  it should "allow A => M[B] and B => M[C] to be composed giving A => M[C]" in {
    (first >=> second) (1).await shouldBe 1.0d
  }
}

class KlesliTestForFuture extends AbstractKleisliTest[Future]
