package one.xingyi.utils.monads

import one.xingyi.utils.UtilsWithLoggingSpec
import one.xingyi.utils._

import scala.concurrent.Future
import scala.language.higherKinds
import one.xingyi.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.utils.functions.{Async, Monad}
import one.xingyi.utils.language.Language._

abstract class AbstractKleisliTest[M[_]: Async:Monad] extends UtilsWithLoggingSpec {

  behavior of "Kleisli"

  val first: Int => M[String] = _.toString.liftM[M]
  val second: String => M[Double] = _.toDouble.liftM[M]

  it should "allow A => M[B] and B => M[C] to be composed giving A => M[C]" in {
    (first |==> second) (1).await() shouldBe 1.0d
  }
}

class KlesliTestForFuture extends AbstractKleisliTest[Future]
