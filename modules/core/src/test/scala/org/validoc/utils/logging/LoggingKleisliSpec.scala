package org.validoc.utils.logging

import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.UtilsSpec
import org.validoc.utils.functions.{Async, Functions, MonadCanFailWithException}

import scala.concurrent.Future
import scala.language.higherKinds

abstract class LoggingKleisliSpec[M[_] : Async, Fail](implicit m: MonadCanFailWithException[M, Fail]) extends UtilsSpec {

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
    val loggingKleisli = new LoggingKleisli[M, Fail] {
      override implicit def monad: MonadCanFailWithException[M, Fail] = m
      override protected val logReqAndResult = new LogRequestAndResult[M, Fail] {
        override def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req) = {
          req shouldBe "input"
          messagePrefix shouldBe "someMessagePrefix"
          sender shouldBe messagePrefix //for now. We may improve this later

          //Need to think about how to test. Might end up splitting Report Data... it has two responsiblities at the moment...
          Functions.identify
        }
      }
    }
    val raw = mock[String => M[String]]
    fn(loggingKleisli.logging[String, String]("")(raw), raw, state)
  }


  it should "pass a logging message to the logReqAndResult when " in {
    setup {
      (logging, raw, remembered) =>
      //      raw.sideeffect(logReqAndResult[Req, Res](messagePrefix, messagePrefix))
    }
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

class ScalaFutureLoggingKleisliSpec extends LoggingKleisliSpec[Future, Throwable]
