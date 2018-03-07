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

  def kleisliTransformer[T1, T2, T3, T4](k1: T1 => M[T2], k2: T3 => M[T4]): (T1 => M[T2]) => (T3 => M[T4]) = {
    actual1 =>
      actual1 shouldBe k1
      k2
  }
}
