package org.validoc.utils.http

import org.validoc.utils.success._

import scala.util.{Failure, Success, Try}

object ServiceResponse {

  implicit object SucceededForTryServiceResponse extends Succeeded[ServiceResponse] {
    def is200Code(status: Status) = (status.code / 100) == 2

    override def apply(serviceResponseTry: Try[ServiceResponse]): SucceededState[ServiceResponse] = serviceResponseTry match {
      case Success(sr@ServiceResponse(status, _, _)) if is200Code((status)) => SuccessState(sr)
      case Success(sr@ServiceResponse(status, _, _)) => FailedState(sr)
      case Failure(t) => ExceptionState(t)
    }
  }

}

case class ServiceResponse(status: Status, body: Body, contentType: ContentType)

trait ToServiceResponse[HttpRes] extends (HttpRes => ServiceResponse)

trait FromServiceResponse[HttpRes] extends (ServiceResponse => HttpRes)
