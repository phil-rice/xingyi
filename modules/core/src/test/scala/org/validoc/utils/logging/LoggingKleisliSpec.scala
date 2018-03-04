package org.validoc.utils.logging

import java.text.MessageFormat
import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.UtilsSpec
import org.validoc.utils.functions.{Async, Functions, MonadCanFailWithException}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Success, Try}
import org.validoc.utils._
import org.validoc.utils.objectify.ServiceResponseFixture
import org.mockito.Mockito._

abstract class LoggingKleisliSpec[M[_] : Async, Fail](implicit m: MonadCanFailWithException[M, Fail]) extends UtilsSpec {

  type StringKleisli = String => M[String]
  behavior of "LoggingKleisli"

  implicit object DetailsLoggingForString extends DetailedLogging[String] {
    override def apply(v1: String) = s"<<<$v1>>>"
  }
  implicit object SummaryLoggingForString extends SummaryLogging[String] {
    override def apply(v1: String) = s"((($v1)))"
  }


  def setup[X](fn: (StringKleisli, StringKleisli, AtomicReference[Try[Either[Fail, _]]]) => X): X = {
    val state = new AtomicReference[Try[Either[Fail, _]]]()
    val loggingKleisli = new LoggingKleisli[M, Fail] {
      override implicit def monad: MonadCanFailWithException[M, Fail] = m
      override protected val logReqAndResult = new LogRequestAndResult[Fail] {
        override def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req) = {
          req shouldBe "input"
          messagePrefix shouldBe "someMessagePrefix"
          sender shouldBe messagePrefix //for now. We may improve this later
          state.set _
        }
        override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) =
          MessageFormat.format(messagePrefix + "." + messagePrefix + "{1}{2}{3}{4}", strings: _*)
      }
    }
    val raw = mock[String => M[String]]
    fn(loggingKleisli.logging[String, String]("someMessagePrefix")(raw), raw, state)
  }


  it should "pass a logging message to the logReqAndResult when " in {
    setup { (logging, raw, remembered) =>
      when(raw.apply("input")) thenReturn "output".liftM
      logging("input").await() shouldBe "output"
      remembered.get shouldBe Success(Right("output"))
    }
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

class ScalaFutureLoggingKleisliSpec extends LoggingKleisliSpec[Future, Throwable]
