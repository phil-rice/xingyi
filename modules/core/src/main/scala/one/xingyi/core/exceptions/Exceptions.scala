package one.xingyi.core.exceptions
import one.xingyi.core.http.{ServiceRequest, ServiceResponse}
import one.xingyi.core.logging.{DetailedLogging, RequestDetails}
import one.xingyi.core.monad.MonadWithException

import scala.language.higherKinds

object Exceptions {
  def apply[M[_], T](m: => M[T])(implicit monadWithException: MonadWithException[M]): M[T] =
    try {
      m
    } catch {
      case e: Throwable => monadWithException.exception(e)
    }
}

class NotFoundException(val req: Any, val response: ServiceResponse) extends Exception(s"Not found: $response")

class UnexpectedStatusCodeException(val req: Any, val response: ServiceResponse) extends Exception(s"unexpected status code: $response")

class EndpointNotFoundException(val serviceRequest: ServiceRequest) extends Exception(serviceRequest.toString)

class ResponseParserException[Req](req: Req, info: String, serviceResponse: ServiceResponse)(implicit reqDetails: DetailedLogging[Req], srDetails: DetailedLogging[ServiceResponse])
  extends Exception(s"$info the response was ${srDetails(serviceResponse)} when request was: ${reqDetails(req)}")