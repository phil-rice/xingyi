package org.validoc.utils.logging

import java.text.MessageFormat

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.http.RequestDetails
import org.validoc.utils.service.MakeServiceMakerForClassWithParam
import org.validoc.utils.success.Succeeded

import scala.language.higherKinds

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

object LoggingService {
  implicit def makeLoggingService[OldService <: Req => M[Res], M[_] : Async, Req, Res: Succeeded] = new MakeServiceMakerForClassWithParam[String, OldService, LoggingService[M, Req, Res]] {
    override def apply(prefix: String, delegate: OldService): LoggingService[M, Req, Res] = new LoggingService[M, Req, Res](delegate, prefix)
  }
}

class LoggingService[M[_] : Async, Req, Res: Succeeded](delegate: Service[M, Req, Res], pattern: String)(implicit loggingStrings: LoggingStrings[Res], loggingAdapter: LoggingAdapter) extends Service[M, Req, Res] with Logging {
  def toRequestDetails(req: Req) = RequestDetails[Req](MessageFormat.format(pattern, req.toString))(req)

  override def apply(req: Req): M[Res] = {
    lazy val requestDetails: RequestDetails[Req] = toRequestDetails(req)
    trace(s"Requesting $requestDetails")
    delegate(req).registerSideEffectWhenComplete(log[Req, Res](requestDetails, _))

  }
}