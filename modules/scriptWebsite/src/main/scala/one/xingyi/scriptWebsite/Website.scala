/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptWebsite

import javax.net.ssl.SSLContext
import one.xingyi.cddmustache.{Mustache, NameToMustacheTemplate}
import one.xingyi.core.client.HttpClient
import one.xingyi.core.endpoint.{DisplayRecordedKleisli, MatchesServiceRequest}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.logging._
import one.xingyi.core.monad._
import one.xingyi.core.objectify._
import one.xingyi.core.service.html.ToHtml
import one.xingyi.core.simpleServer.CheapServer
import one.xingyi.scriptExample.createdCode1.Model1Domain
import org.json4s.JValue

import scala.language.higherKinds

case class MustacheToHtml[J: JsonWriter, T](templateName: String, title: String)(implicit toJsonLib: ToJsonLib[T], nameToMustacheTemplate: NameToMustacheTemplate) extends ToHtml[T] {
  val mf = Mustache(title, templateName, "main.template.mustache")

  override def apply(t: T): String = mf.apply(JsonMaps(toJsonLib(t)))
}

class Website[M[_] : Async, Fail: Failer : LogRequestAndResult, J: JsonParser : JsonWriter]
(implicit val monad: MonadCanFailWithException[M, Fail] with MonadWithState[M], val logReqAndResult: LogRequestAndResult[Fail], loggingAdapter: LoggingAdapter)
  extends CheapServer[M, Fail](9000) with XingyiKleisli[M, Fail] with RecordCallsKleisli[M, Fail] with DisplayRecordedKleisli[M] {

  implicit val ssl: Option[SSLContext] = None
  private val domain: Domain = Domain(Protocol("http"), HostName("localhost"), Port(9001))
  override implicit lazy val httpFactory = new HttpFactory[M, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = HttpClient.apply[M](domain)
  }

  implicit val template = Mustache("Demo", "personAndRequests.mustache", "main.template.mustache")

  implicit val ToHtmlForIndex = MustacheToHtml[J, IndexPageResponse]("index.mustache", "Xing Yi demo")
  implicit val ToHtmlForRecordedCalls = MustacheToHtml[J, ResultWithRecordedCalls[ServiceResponse]]("personAndRequests.mustache", "Xing Yi demo")


  val keepalive: ServiceRequest => M[Option[ServiceResponse]] = sr => Option(ServiceResponse("Alive")).liftM

  implicit val recordedCalls = LocalVariable[RecordedCall]
  val index = function[IndexPageRequest, IndexPageResponse]("index")(_ => IndexPageResponse()) |+| endpoint[IndexPageRequest, IndexPageResponse]("/", MatchesServiceRequest.fixedPath(Method("get")))
  val person = http(ServiceName("Backend")) |+| recordCalls |+| xingyify[PersonAddressRequest, PersonAddressResponse](Model1Domain) |+| endpoint[PersonAddressRequest, PersonAddressResponse]("/person", MatchesServiceRequest.idAtEnd(Method("get"))) |+| andDisplayRecorded[J]

  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(index, person, keepalive)

}

object Website extends App {

  import one.xingyi.json4s.Json4sParser._
  import one.xingyi.json4s.Json4sWriter._

  implicit val logger: LoggingAdapter = PrintlnLoggingAdapter

  import SimpleLogRequestAndResult._

  println("Checking backend")


  val website = new Website[IdentityMonad, Throwable, JValue]

  println("running")
  website.start
}
