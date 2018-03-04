package org.validoc.utils.logging

import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.UtilsSpec
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.success.{SucceededState, SuccessState}

import scala.language.higherKinds

class LoggingKleisliSpec[M[_] : Async, Fail](implicit val monad: MonadCanFailWithException[M, Fail]) extends UtilsSpec with LoggingKleisli[M, Fail] {

  type StringKleisli = String => M[String]
  behavior of "LoggingKleisli"

  implicit object DetailsLoggingForString extends DetailedLogging[String] {
    override def apply(v1: String) = s"<<<$v1>>>"
  }
  implicit object SummaryLoggingForString extends SummaryLogging[String] {
    override def apply(v1: String) = s"((($v1)))"
  }


  def setup[X](fn: (StringKleisli, StringKleisli, AtomicReference[Any]) => X): X = {
    val state = new AtomicReference[Any]()
    implicit val logRequestAndResult: LogRequestAndResult[Fail] = new LogRequestAndResult[Fail] {
      override def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req): SucceededState[Fail, Res] => Unit = {
        req shouldBe "input"
        messagePrefix shouldBe "someMessagePrefix"
        sender shouldBe messagePrefix //for now. We may improve this later

        {
          case SuccessState(x) => x shouldBe "result"
          case _ => fail
        }
      }
    }
    val raw = mock[Kleisli[String, String]]
    val x = logging[String, String]("")(raw)
    fn(x, raw, state)
  }

  it should "" in {

    fail
  }
}
