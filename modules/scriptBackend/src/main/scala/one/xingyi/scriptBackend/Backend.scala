/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend

import java.util.ResourceBundle
import java.util.concurrent.Executors

import one.xingyi.core.aggregate.{EnrichLanguage, MergeLanguage}
import one.xingyi.core.cache.{CacheFactory, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.endpoint.{EndPoint, MatchesServiceRequest}
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter, ToJson, ToJsonLib}
import one.xingyi.core.language._
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult, PrintlnLoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.{PrintlnPutMetrics, PutMetrics}
import one.xingyi.core.monad.{Async, IdentityMonad, MonadCanFailWithException}
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.time.NanoTimeService
import org.json4s.JValue

import scala.language.higherKinds
import scala.util.Success



class Backend[M[_], Fail, J: JsonParser : JsonWriter](implicit val monad: MonadCanFailWithException[M, Fail],
                                                      val async: Async[M],
                                                      val failer: Failer[Fail],
                                                      val cacheFactory: CacheFactory[M],
                                                      val timeService: NanoTimeService,
                                                      val logReqAndResult: LogRequestAndResult[Fail],
                                                      val putMetrics: PutMetrics,
                                                      val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                                      val detailedLoggingForSR: DetailedLogging[ServiceResponse])
  extends MicroserviceBuilder[M, Fail] with MicroserviceComposers[M] with EnrichLanguage[M] with MergeLanguage[M] with AnyLanguage with MonadLanguage with AsyncLanguage {

  val tel = Telephone("someNumber")
  val address1 = Address("line1", "line2", "pc1")
  val address2 = Address("line2", "line2", "pc2")
  val person = Person("someName", List(address1, address2), tel)
  var people = Map[String, Person]("someName"-> person)

  val x = implicitly[ToJsonLib[Person]]
  val y: ToJson[Person] = ToJson.default[J, Person]
  val z = ToServiceResponse.toServiceResponse[Person]

  val getPerson = function[PersonRequest, Person]("findPerson")(req => people.getOrElse(req.name, throw new RuntimeException("not found"))) |+| endpoint[PersonRequest, Person]("/person", MatchesServiceRequest.idAtEnd(Method("get")))
  val editPerson = function[EditPersonRequest, Person]("findPerson"){req => people = people + (req.person.name -> req.person); req.person} |+| endpoint[EditPersonRequest, Person]("/person", MatchesServiceRequest.idAtEnd(Method("post")))

  //  val getPerson = function[PersonRequest, Person]("findPerson")(req => people.find(_ == req.name).getOrElse(throw new RuntimeException("not found"))) |+| endpoint[PersonRequest, Person]("/person", MatchesServiceRequest.idAtEnd(Method("get")))
  //  ;
  val keepalive: ServiceRequest => M[Option[ServiceResponse]] = function[ServiceRequest, ServiceResponse]("keepalive")(sr => ServiceResponse("Alive")) |+| endpoint[ServiceRequest, ServiceResponse]("/ping", MatchesServiceRequest.fixedPath(Method("get")))


  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(getPerson,editPerson, keepalive)


}

object Backend extends App {
  import one.xingyi.json4s.Json4sParser._
  import one.xingyi.json4s.Json4sWriter._
  implicit val httpFactory = new HttpFactory[IdentityMonad, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req =>
      IdentityMonad(Success(ServiceResponse(Status(200), Body(s"response: ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))), Map())
    }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new SimpleLogRequestAndResult
  implicit val cacheFactory = new CachingServiceFactory[IdentityMonad](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)

  implicit val executors = Executors.newFixedThreadPool(10)

  import one.xingyi.core.http.Failer.failerForThrowable


  val backend = new Backend[IdentityMonad, Throwable, JValue]
  val server = new SimpleHttpServer(9001, new EndpointHandler[IdentityMonad, Throwable](backend.endpoints))

  println("running")
  server.start()
}
