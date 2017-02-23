package org.validoc.utils.monads

import org.validoc.utils.UtilsSpec
import org.validoc.utils.concurrency.{Async, MDCPropagatingExecutionContext}

import scala.concurrent.Future

abstract class AbstractKleisliTest[M[_]] extends UtilsSpec {
  implicit val async: Async[M]
  behavior of "Kleisli"

  import Kleisli._
  import org.validoc.utils.concurrency.Async._

  val first: Int => M[String] = _.toString.liftValue[M]
  val second: String => M[Double] = _.toDouble.liftValue[M]

  it should "allow A => M[B] and B => M[C] to be composed giving A => M[C]" in {
    (first >=> second) (1).await shouldBe 1.0d
  }
}

class KlesliTestForFuture extends AbstractKleisliTest[Future] {
  override implicit val async: Async[Future] = Async.asyncForFuture
}
