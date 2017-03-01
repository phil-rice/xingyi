package org.validoc.utils.logging

import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.http.RequestDetails
import org.validoc.utils.success.{Succeeded, SucceededFromFn}

import scala.concurrent.Future
import scala.util.Success


class LoggingServiceTests extends UtilsWithLoggingSpec with LoggingFixture {

  behavior of "LoggingService"

  implicit def someLoggingStrings[Res] = new LoggingStrings[Res] {
    override def succeeded(req: RequestDetails[_], res: Res) = (Trace, s"Success: ${req.req} with $res")

    override def failed(req: RequestDetails[_], res: Res) = (Info, s"Failed:  $req with $res")

    override def exception(req: RequestDetails[_], t: Throwable) = (Error, s"Exception  from $req")
  }

  val runtimeException = new RuntimeException

  def setup(fn: LoggingService[Future, String, String] => LoggingMemoriseForTests => LoggingAdapter => Unit): Unit = {
    implicit val suceeded: Succeeded[String] = new SucceededFromFn[String](_ contains "success")

    val delegate = { x: String =>
      x match {
        case "success" => Future(x + "_result")
        case _ => throw runtimeException
      }
    }

    implicit val memoriseForTests = new LoggingMemoriseForTests
    implicit val loggingAdapter = NullLoggingAdapterWithMdc
    fn(new LoggingService[Future, String, String](delegate, "[{0}]"))(memoriseForTests)(loggingAdapter)
  }

  it should "log at trace level at the start, and then the result of the logging strings when succeeds" in {
    setup { service =>
      loggingMemoriser =>
        implicit loggingAdapter =>
          val LoggingReport(Success("success_result"), records) = await(loggingMemoriser.traceFuture(service("success")))
          records shouldBe "LoggingRecorda"
    }
  }

}
