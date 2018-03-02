package org.validoc.utils

import org.scalatest.Matchers
import org.validoc.utils.functions.{Async, MonadWithException}

import scala.language.higherKinds
import scala.util.Try
import org.validoc.utils.language.Language._

trait AsyncFixture[M[_]] extends Matchers {

  def kleisli[Req, Res](expected: Req, result: => Try[Res])(implicit async: MonadWithException[M]): Req => M[Res] = { req: Req =>
    req shouldBe expected
    liftTry(result)
  }
}
