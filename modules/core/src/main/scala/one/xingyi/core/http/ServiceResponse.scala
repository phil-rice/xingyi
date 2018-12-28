/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.exceptions.UnexpectedStatusCodeException
import one.xingyi.core.functions.Functions
import one.xingyi.core.json._
import one.xingyi.core.profiling.{DontProfile, ProfileAs, ProfileAsFail, ProfileAsSuccess}
import one.xingyi.core.reflection.ClassTags
import one.xingyi.core.script.{IXingYi, IXingYiLoader}
import one.xingyi.core.success._

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

case class ServiceResponse(status: Status, body: Body, headers: List[Header]) {
  private def getHeader[H: ClassTag]: Option[H] = ClassTags.collectAll[H](headers).headOption
   def findHeader(name: String): Option[String] = headers.find(h=>name.equalsIgnoreCase(h.name)).map(_.value)

  def contentType: Option[ContentType] = getHeader[ContentType]
}


object ServiceResponse extends JsonWriterLanguage {
  def apply(html: String): ServiceResponse = ServiceResponse(Status(200), Body(html), ContentType("text/html"))

  def apply(status: Status, body: Body, contentType: ContentType): ServiceResponse = new ServiceResponse(status, body, List(contentType))

  def removeHeader(name: String)(serviceResponse: ServiceResponse) = serviceResponse.copy(headers = serviceResponse.headers.filterNot(_.name == name))

  def serviceResponseToXingYiCodeAndBody(sr: ServiceResponse): (String, String) = {
    val body = sr.body.s
    val index = body.indexOf("=")
    if (index == -1) throw new RuntimeException("The response from server is not a XingYi payload\n" + sr)
    val code = body.substring(0, index + 1)
    (code, body.substring(index + 1))
  }

  def bodyToJson(serviceResponse: ServiceResponse): (String,JsonValue) = {
    serviceResponse.findHeader("content-type") match {
      case Some(ct) if ct.startsWith("application/xingyi") =>
        val (code, body) = serviceResponseToXingYiCodeAndBody(serviceResponse)
        "xingyi"-> JsonObject("code" -> code, "body" -> body)
      case Some(ct) if ct.startsWith("application/json") => "jsonBody" -> serviceResponse.body.s
      case _ => "body"-> serviceResponse.body.s
    }
  }

  implicit def toJsonLib(implicit headerToJson: ToJsonLib[Seq[Header]]): ToJsonLib[ServiceResponse] =
    sr => JsonObject("status" -> sr.status.code, bodyToJson(sr), "headers" -> headerToJson(sr.headers))

}

@implicitNotFound(
  """Missing ToServiceResponse[${T}] This turns ${T} into a service response so that it can be shown to the user. The simplest way to implement this is to have a 'ToJson[${T}]' in the scope.
    To debug this you can have the following code
    val x = implicitly[ToJson[${T}]]
    val y = implicitly[ToServiceResponse]

  """)
trait ToServiceResponse[Req, T] extends (Req => T => ServiceResponse)

object ToServiceResponse {

  implicit def toServiceResponse[Req, T](implicit toJson: ToJson[T]) = new ToServiceResponse[Req, T] {
    override def apply(req: Req): T => ServiceResponse = t => ServiceResponse(Status.Ok, Body(toJson(t)), ContentType("application/json"))
  }

  implicit def ToServiceResponseForServiceResponse[Req]: ToServiceResponse[Req, ServiceResponse] = req => sr => sr

}

@implicitNotFound("Missing FromServiceResponse[${T}] This creates a(${T}) from a service response returned by a client call. The simplest way to implement this is to have the domain object companion extend DomainCompanionObject and have a 'FromJson[${T}]' in the scope. This allows all decisions about which JSON library  we are using to be dealt with outside the main business logic")
trait FromServiceResponse[T] extends (ServiceResponse => T)

object FromServiceResponse extends JsonWriterLanguage {

  implicit object FromServiceResponseForServiceResponse extends FromServiceResponse[ServiceResponse] {
    override def apply(v1: ServiceResponse): ServiceResponse = v1
  }

  implicit def toJsonLib(implicit headerToJson: ToJsonLib[Seq[Header]]): ToJsonLib[ServiceResponse] =
    sr => JsonObject("status" -> sr.status.code,
      "body" -> sr.body.s,
      "headers" -> headerToJson(sr.headers))

}


trait FromXingYi[Req, Res] extends (IXingYi => Req => String => Res)

trait EndpointPath[T] extends (ServiceResponse => Option[T])

