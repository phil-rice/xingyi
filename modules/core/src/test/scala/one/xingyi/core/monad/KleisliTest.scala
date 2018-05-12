package one.xingyi.core.monad

import one.xingyi.core.UtilsWithLoggingSpec
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

import scala.concurrent.Future
import scala.language.higherKinds

abstract class AbstractKleisliTest[M[_]: Async:Monad] extends UtilsWithLoggingSpec {

  behavior of "Kleisli"

  val first: Int => M[String] = _.toString.liftM[M]
  val second: String => M[Double] = _.toDouble.liftM[M]

  it should "allow A => M[B] and B => M[C] to be composed giving A => M[C]" in {
    (first |==> second) (1).await() shouldBe 1.0d
  }
}

class KlesliTestForFuture extends AbstractKleisliTest[Future]
