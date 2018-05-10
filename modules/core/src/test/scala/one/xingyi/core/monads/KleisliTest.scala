package one.xingyi.core.monads

import one.xingyi.core.UtilsWithLoggingSpec
import one.xingyi.core._

import scala.concurrent.Future
import scala.language.higherKinds
import one.xingyi.core.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.core.functions.{Async, Monad}
import one.xingyi.core.language.Language._

abstract class AbstractKleisliTest[M[_]: Async:Monad] extends UtilsWithLoggingSpec {

  behavior of "Kleisli"

  val first: Int => M[String] = _.toString.liftM[M]
  val second: String => M[Double] = _.toDouble.liftM[M]

  it should "allow A => M[B] and B => M[C] to be composed giving A => M[C]" in {
    (first |==> second) (1).await() shouldBe 1.0d
  }
}

class KlesliTestForFuture extends AbstractKleisliTest[Future]
