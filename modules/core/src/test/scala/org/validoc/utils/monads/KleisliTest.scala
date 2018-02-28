package org.validoc.utils.monads

import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.concurrency.Async
import org.validoc.utils._

import scala.concurrent.Future
import scala.language.higherKinds
import org.validoc.utils.concurrency.AsyncForScalaFuture._
import org.validoc.utils.concurrency.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.Monad

abstract class AbstractKleisliTest[M[_]: Async:Monad] extends UtilsWithLoggingSpec {

  behavior of "Kleisli"

  val first: Int => M[String] = _.toString.liftM[M]
  val second: String => M[Double] = _.toDouble.liftM[M]

  it should "allow A => M[B] and B => M[C] to be composed giving A => M[C]" in {
    (first |==> second) (1).await() shouldBe 1.0d
  }
}

class KlesliTestForFuture extends AbstractKleisliTest[Future]
