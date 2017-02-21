package org.validoc.utils.logging

import java.text.MessageFormat

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import Async._
import org.validoc.utils.http.RequestDetails
import org.validoc.utils.success.{ExceptionState, FailedState, Succeeded, SuccessState}

trait LoggingStrings[Res] {
  def succeeded(req: RequestDetails[_], res: Res): String

  def failed(req: RequestDetails[_], res: Res): String

  def exception(req: RequestDetails[_], t: Throwable): String
}

object LoggingStrings {
  implicit def defaultLoggingStrings[Res] = new LoggingStrings[Res] {
    override def succeeded(req: RequestDetails[_], res: Res): String = s"Success: Returned from $req with $res"

    override def failed(req: RequestDetails[_], res: Res): String = s"Failed: Returned from $req with $res"

    override def exception(req: RequestDetails[_], t: Throwable): String = s"Exception returning from $req"
  }

}


class LoggingService[M[_] : Async, Req, Res: Succeeded](delegate: Service[M, Req, Res], pattern: String)(implicit loggingStrings: LoggingStrings[Res]) extends Service[M, Req, Res] with Logging {
  def toRequestDetails = RequestDetails[Req](MessageFormat.format(pattern)) _

  override def apply(req: Req): M[Res] = {
    lazy val requestDetails: RequestDetails[Req] = toRequestDetails(req)
    trace(s"Requesting $requestDetails")

    val result = delegate(req)

    result.registerSideEffectWhenComplete { tryT =>
      implicitly[Succeeded[Res]].apply(tryT) match {
        case SuccessState(res) => trace(loggingStrings.succeeded(requestDetails, res))
        case FailedState(res) => debug(loggingStrings.failed(requestDetails, res))
        case ExceptionState(t) => error(loggingStrings.exception(requestDetails, t), t)
      }

    }
  }
}
