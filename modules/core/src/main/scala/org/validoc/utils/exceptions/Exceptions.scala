package org.validoc.utils.exceptions

import org.validoc.utils.functions.MonadWithException
import org.validoc.utils.http.{ServiceRequest, ServiceResponse}

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