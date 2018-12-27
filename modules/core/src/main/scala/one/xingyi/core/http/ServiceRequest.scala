/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import com.sun.net.httpserver.HttpExchange
import one.xingyi.core.json.{JsonList, JsonObject, ToJsonLib}
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Liftable
import one.xingyi.core.reflection.ClassTags
import one.xingyi.core.simpleServer.Streams

import scala.annotation.implicitNotFound
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag

case class ServiceRequest(method: Method, domain: Option[Domain], path: Path, params: Seq[QueryParam], headers: Seq[Header], body: Option[Body]) {
  private def getHeader[H: ClassTag]: Option[H] = ClassTags.collectAll[H](headers).headOption

  lazy val contentType: Option[ContentType] = getHeader[ContentType]
  lazy val accept: Option[AcceptHeader] = getHeader[AcceptHeader]
  lazy val uri = Uri(domain, path, params: _*)

  def header(name: String) = headers.find(_.name.equalsIgnoreCase(name)).map(_.value)
}

object ServiceRequest {
  def apply(method: Method, uri: Uri, headers: Seq[Header], body: Option[Body]): ServiceRequest =
    new ServiceRequest(method, uri.domain, uri.path, uri.params, headers, body)

  def apply(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, contentType: Option[ContentType] = None, otherHeaders: List[Header] = List(), body: Option[Body] = None): ServiceRequest =
    new ServiceRequest(method, uri.domain, uri.path, uri.params, otherHeaders + acceptHeader + contentType, body)

  def removeHeader(name: String)(serviceRequest: ServiceRequest) = serviceRequest.copy(headers = serviceRequest.headers.filterNot(_.name == name))

  implicit def toJsonLib(implicit headerToJson: ToJsonLib[Seq[Header]]): ToJsonLib[ServiceRequest] =
    sr => JsonObject("method" -> sr.method.toString, "uri" -> sr.uri.asUriString, "headers" -> headerToJson(sr.headers)) optionalAdd("body", sr.body.map(_.s))

}

@implicitNotFound("""Missing ToServiceRequest[${T}] This is how we turn a query/request object (${T}) into a HTTP request. If ${T} is a http request have """)
trait ToServiceRequest[T] extends (T => ServiceRequest)

object ToServiceRequest {

  implicit object ToServiceRequestForServiceRequest extends ToServiceRequest[ServiceRequest] {
    override def apply(v1: ServiceRequest): ServiceRequest = v1
  }

  implicit object ToServiceRequestForHttpExchange extends ToServiceRequest[HttpExchange] {
    override def apply(httpExchange: HttpExchange): ServiceRequest = {
      val method = Method(httpExchange.getRequestMethod.toLowerCase())
      val bodyString = Streams.readAll(httpExchange.getRequestBody)
      val body = if (bodyString == "") None else Some(Body(bodyString))
      val uri = Uri(httpExchange.getRequestURI.toString)
      val javaHeaders: com.sun.net.httpserver.Headers = httpExchange.getRequestHeaders;
      val headers: immutable.Seq[Header] = javaHeaders.keySet().asScala.foldLeft(List[Header]())((acc, k) => javaHeaders.get(k).asScala.foldLeft(acc)((acc2, v) => Header(k.toLowerCase, v) :: acc2)).reverse
      ServiceRequest(method, uri, headers, body)
    }
  }

}

@implicitNotFound("Missing FromServiceRequest[${T}]This is how we create a query/request (${T}) from an external clients HTTP request. It is isolated from exactly which webframework we are using.")
trait FromServiceRequest[M[_], T] extends (ServiceRequest => M[T])

object FromServiceRequest {
  implicit def FromServiceResponseForServiceResponse[M[_] : Liftable] = new FromServiceRequest[M, ServiceRequest] {
    override def apply(v1: ServiceRequest): M[ServiceRequest] = v1.liftM
  }
}
