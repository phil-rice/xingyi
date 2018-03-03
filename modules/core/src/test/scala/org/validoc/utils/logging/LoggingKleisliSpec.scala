package org.validoc.utils.logging

import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.UtilsSpec
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.success.{MessageName, SucceededState}

import scala.language.higherKinds
import scala.reflect.ClassTag

class LoggingKleisliSpec[M[_] : Async, Fail](implicit val monad: MonadCanFailWithException[M, Fail]) extends UtilsSpec with LoggingKleisli[M, Fail] {

  type StringKleisli = String => M[String]
  behavior of "LoggingKleisli"

  implicit object DetailsLoggingForString extends DetailedLogging[String] {
    override def apply(v1: String) = s"<<<$v1>>>"
  }
  implicit object SummaryLoggingForString extends SummaryLogging[String] {
    override def apply(v1: String) = s"((($v1)))"
  }

  implicit val someMessageName = MessageName[String, String]("someMessageName")

  implicit val someSender = mock[AnyRef]

  def setup[X](fn: (StringKleisli, StringKleisli, AtomicReference[Any]) => X): X = {
    val state = new AtomicReference[Any]()
    implicit val logRequestAndResult: LogRequestAndResult[Fail] = new LogRequestAndResult[Fail] {
      override def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any)(req: Req)(implicit messageName: MessageName[Req, Res]): SucceededState[Fail, Res] => Unit = {
        req shouldBe "input"
        messageName shouldBe someMessageName
        sender shouldBe someSender

        { s => state.set(s) }
      }
    }
    val raw = mock[Kleisli[String, String]]
    val x = logging[String, String]("")(raw)
    fn(x, raw, state)
  }

  it should "" in {

  }
}
