/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptWebsite

import java.util.ResourceBundle
import java.util.concurrent.Executors

import javax.net.ssl.SSLContext
import one.xingyi.core.aggregate.{EnrichLanguage, MergeLanguage}
import one.xingyi.core.cache.{CacheFactory, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.client.HttpClient
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language._
import one.xingyi.core.logging._
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.{PrintlnPutMetrics, PutMetrics}
import one.xingyi.core.monad.{Async, IdentityMonad, MonadCanFailWithException}
import one.xingyi.core.objectify.{EntityDetailsRequest, EntityDetailsResponse}
import one.xingyi.core.script.IXingYiLoader
import one.xingyi.core.simpleServer.{CheapServer, EndpointHandler, SimpleHttpServer}
import one.xingyi.core.strings.Strings
import one.xingyi.core.time.NanoTimeService
import one.xingyi.scriptExample.createdCode.{Person, PersonLine12Ops}
import org.json4s.JValue

import scala.io.Source
import scala.language.higherKinds


class Website[M[_] : Async, Fail: Failer : LogRequestAndResult, J: JsonParser : JsonWriter]
(implicit val monad: MonadCanFailWithException[M, Fail], val logReqAndResult: LogRequestAndResult[Fail], loggingAdapter: LoggingAdapter)
  extends CheapServer[M, Fail](9000) {

  implicit val ssl: Option[SSLContext] = None
  private val domain: Domain = Domain(Protocol("http"), HostName("localhost"), Port(9001))
  override implicit lazy val httpFactory = new HttpFactory[M, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = HttpClient.apply[M](domain)
  }

  val javascript = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("example.js")).mkString
  implicit val xingyi = implicitly[IXingYiLoader].apply(javascript)

  val keepalive: ServiceRequest => M[Option[ServiceResponse]] = sr => Option(ServiceResponse("Alive")).liftM


  val person = http(ServiceName("Backend")) |+| objectify[PersonAddressRequest, PersonAddressResponse] |+| endpoint[PersonAddressRequest, PersonAddressResponse]("/personold", MatchesServiceRequest.idAtEnd(Method("get")))
  val person2 = http(ServiceName("Backend")) |+| xingyi[PersonAddressRequest, PersonAddressResponse] |+| endpoint[PersonAddressRequest, PersonAddressResponse]("/person", MatchesServiceRequest.idAtEnd(Method("get")))

  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain( person2, keepalive)

  val client: ServiceRequest => M[ServiceResponse] = httpFactory(ServiceName("Backend"))
  println("found: " + implicitly[Async[M]].await(client(ServiceRequest(Method("get"), Uri("http://localhost:9001/person/someName")))))


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
