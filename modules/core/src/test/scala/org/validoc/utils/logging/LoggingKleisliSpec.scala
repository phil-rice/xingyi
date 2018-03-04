package org.validoc.utils.logging

import java.text.MessageFormat
import java.util.concurrent.atomic.AtomicReference

import org.mockito.Mockito._
import org.validoc.utils.{UtilsSpec, _}
import org.validoc.utils.functions.{Async, MonadCanFailWithException}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Success, Try}

abstract class LoggingKleisliWhenNoRealFailSpec[M[_] : Async, Fail](implicit m: MonadCanFailWithException[M, Fail]) extends UtilsSpec with FunctionFixture {

  type StringKleisli = String => M[String]
  behavior of "LoggingKleisli"

  implicit object DetailsLoggingForString extends DetailedLogging[String] {
    override def apply(v1: String) = s"+$v1+"
  }
  implicit object SummaryLoggingForString extends SummaryLogging[String] {
    override def apply(v1: String) = s"-$v1-"
  }


  def setup[X](setupFn: (StringKleisli, StringKleisli, RememberLoggingAdapter) => X): X = {
    implicit val loggingAdapter = new RememberLoggingAdapter
    val loggingKleisli = new LoggingKleisli[M, Fail] {
      override implicit def monad: MonadCanFailWithException[M, Fail] = m
      override protected val logReqAndResult = new AbstractLogRequestAndResult[Fail] {
        override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) =
          MessageFormat.format(messagePrefix + "." + messagePrefix + "{0}{1}{2}{3}{4}", strings: _*)
      }
    }
    val raw = mock[String => M[String]]
    setupFn(loggingKleisli.logging[String, String]("someMessagePrefix")(raw), raw, loggingAdapter)
  }


  it should "pass a logging message to the logReqAndResult when succeeds " in {
    setup { (logging, raw, loggingAdapter) =>
      when(raw.apply("input")) thenReturn "output".liftM
      logging("input").await() shouldBe "output"
      loggingAdapter.records shouldBe List(LoggingRecord(0, "DEBUG", "someMessagePrefix/someMessagePrefix.someMessagePrefix-input-+input+-output-+output+{4}", None))
    }
  }
  it should "pass a logging message to the logReqAndResult when exception " in {
    setup { (logging, raw, loggingAdapter) =>
      val exception = new RuntimeException("someMessage")
      when(raw.apply("input")) thenReturn exception.liftException[M, String]
      intercept[RuntimeException](logging("input").await()) shouldBe exception
      loggingAdapter.records shouldBe List(LoggingRecord(0, "ERROR", "someMessagePrefix/someMessagePrefix.someMessagePrefix-input-+input+java.lang.RuntimeException: someMessagejava.lang.RuntimeException: someMessage{4}", Some(exception)))
    }
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

class ScalaFutureLoggingKleisliSpec extends LoggingKleisliWhenNoRealFailSpec[Future, Throwable]
