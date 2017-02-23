package org.validoc.utils.logging

import java.text.MessageFormat
import java.util.logging.Level

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import Async._
import org.validoc.utils.http.RequestDetails
import org.validoc.utils.success.{ExceptionState, FailedState, Succeeded, SuccessState}

trait LoggingStrings[Res] {
  def succeeded(req: RequestDetails[_], res: Res): (LogLevel, String)

  def failed(req: RequestDetails[_], res: Res): (LogLevel, String)

  def exception(req: RequestDetails[_], t: Throwable): (LogLevelThatHasError, String)
}

object LoggingStrings {
  implicit def defaultLoggingStrings[Res] = new LoggingStrings[Res] {
    override def succeeded(req: RequestDetails[_], res: Res) = (Trace, s"Success: Returned from $req with $res")

    override def failed(req: RequestDetails[_], res: Res) = (Error, s"Failed: Returned from $req with $res")

    override def exception(req: RequestDetails[_], t: Throwable) = (Error, s"Exception returning from $req")
  }

}


class LoggingService[M[_] : Async, Req, Res: Succeeded](delegate: Service[M, Req, Res], pattern: String)(implicit loggingStrings: LoggingStrings[Res]) extends Service[M, Req, Res] with Logging {
  def toRequestDetails = RequestDetails[Req](MessageFormat.format(pattern)) _

  override def apply(req: Req): M[Res] = {
    lazy val requestDetails: RequestDetails[Req] = toRequestDetails(req)
    trace(s"Requesting $requestDetails")
    delegate(req).registerSideEffectWhenComplete(log[Req, Res](requestDetails, _))

  }
}