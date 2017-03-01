package org.validoc.utils.logging

import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.http.RequestDetails
import org.validoc.utils.success.{Succeeded, SucceededFromFn}

import scala.concurrent.Future
import scala.util.{Failure, Success}


class LoggingServiceTests extends UtilsWithLoggingSpec with LoggingFixture {

  behavior of "LoggingService"

  implicit def someLoggingStrings[Res] = new LoggingStrings[Res] {
    override def succeeded(req: RequestDetails[_], res: Res) = (Trace, s"Success: ${req.req} with $res")

    override def failed(req: RequestDetails[_], res: Res) = (Info, s"Failed:  $req with $res")

    override def exception(req: RequestDetails[_], t: Throwable) = (Error, s"Exception $t from $req")
  }

  val runtimeException = new RuntimeException

  def setup(fn: LoggingService[Future, String, String] => LoggingMemoriser => LoggingAdapter => Unit): Unit = {
    implicit val suceeded: Succeeded[String] = new SucceededFromFn[String](_ contains "success")

    val delegate = { x: String =>
      x match {
        case "failure" => Future.failed(runtimeException)
        case _ => Future(x + "_result")
      }
    }

    implicit val loggingAdapter = NullLoggingAdapterWithMdc
    fn(new LoggingService[Future, String, String](delegate, "[{0}]"))(LoggingMemoriser)(loggingAdapter)
  }

  it should "log at trace level at the start, and then the result of the logging strings when succeeds" in {
    setup { service =>
      loggingMemoriser =>
        implicit loggingAdapter =>
          val LoggingReport(Success("success_result"), LoggingRecords(records, spans)) = await(loggingMemoriser.traceFuture(service("success")))
          records.map(x => x.msg + "/" + x.level) shouldBe Vector("Requesting RequestDetails(success,Calling [success] with success)/Trace", "Success: success with success_result/Trace")
    }
  }

  it should "log at trace level at the start, and then the result of the logging strings when fails" in {
    setup { service =>
      loggingMemoriser =>
        implicit loggingAdapter =>
          val LoggingReport(Success("someFail_result"), LoggingRecords(records, spans)) = await(loggingMemoriser.traceFuture(service("someFail")))
          records.map(x => x.msg + "/" + x.level) shouldBe Vector("Requesting RequestDetails(someFail,Calling [someFail] with someFail)/Trace", "Failed:  RequestDetails(someFail,Calling [someFail] with someFail) with someFail_result/Trace")
    }
  }

  it should "log at trace level at the start, and then the result of the logging strings when exception thrown" in {
    setup { service =>
      loggingMemoriser =>
        implicit loggingAdapter =>
          val LoggingReport(Failure(e), LoggingRecords(records, spans)) = await(loggingMemoriser.traceFuture(service("failure")))
          e shouldBe runtimeException
          records.map(x => x.msg + "/" + x.level) shouldBe Vector("Requesting RequestDetails(failure,Calling [failure] with failure)/Trace", "Exception java.lang.RuntimeException from RequestDetails(failure,Calling [failure] with failure)/Trace")
    }
  }
}
