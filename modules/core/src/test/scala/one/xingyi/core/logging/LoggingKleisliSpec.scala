/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.logging

import java.text.MessageFormat

import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, MonadCanFailWithException}
import one.xingyi.core.time.MockTimeService
import one.xingyi.core.{UtilsSpec, _}
import org.mockito.Mockito._

import scala.concurrent.Future
import scala.language.higherKinds

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
    implicit val nanoTimeService = new MockTimeService
    implicit val loggingAdapter = new RememberLoggingAdapter
    implicit val logReqAndResult = new AbstractLogRequestAndResult[Fail] {
      override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) =
        MessageFormat.format(messagePrefix + "." + messagePrefix + "{0}{1}{2}{3}{4}", strings: _*)
    }
    val loggingKleisli = new LoggingKleisli[M, Fail] {
    }
    val raw = mock[String => M[String]]
    setupFn(loggingKleisli.logging[String, String]("someMessagePrefix")(raw), raw, loggingAdapter)
  }


  it should "pass a logging message to the logReqAndResult when succeeds " in {
    setup { (logging, raw, loggingAdapter) =>
      when(raw.apply("input")) thenReturn "output".liftM
      logging("input").await() shouldBe "output"
      loggingAdapter.records shouldBe List(LoggingRecord(100, "DEBUG", "someMessagePrefix/someMessagePrefix.someMessagePrefix-input-+input+-output-+output+{4}", None))
    }
  }
  it should "pass a logging message to the logReqAndResult when exception " in {
    setup { (logging, raw, loggingAdapter) =>
      val exception = new RuntimeException("someMessage")
      when(raw.apply("input")) thenReturn exception.liftException[M, String]
      intercept[RuntimeException](logging("input").await()) shouldBe exception
      loggingAdapter.records shouldBe List(LoggingRecord(100, "ERROR", "someMessagePrefix/someMessagePrefix.someMessagePrefix-input-+input+java.lang.RuntimeException: someMessagejava.lang.RuntimeException: someMessage{4}", Some(exception)))
    }
  }
}

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class ScalaFutureLoggingKleisliSpec extends LoggingKleisliWhenNoRealFailSpec[Future, Throwable]
