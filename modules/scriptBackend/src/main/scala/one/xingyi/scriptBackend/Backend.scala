/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend

import java.util.ResourceBundle
import java.util.concurrent.Executors

import one.xingyi.core.aggregate.{EnrichLanguage, MergeLanguage}
import one.xingyi.core.cache.{CacheFactory, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.language._
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult, PrintlnLoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.{PrintlnPutMetrics, PutMetrics}
import one.xingyi.core.monad.{Async, IdentityMonad, MonadCanFailWithException}
import one.xingyi.core.script._
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
  var people = Map[String, Person]("someName" -> person)


  def edit(name: String, person: Person, xingYiHeader: Option[String])(implicit domainList: DomainList[Person]) = {
    people = people + (name -> person)
    println(s"changing $name people now $people header was $xingYiHeader" )
    ServerPayload(Status(200), person, domainList.accept(xingYiHeader))
  }

  //  val q: ToJsonLib[ServerPayload[Person]] = ServerPayload.toJson[Person]
  //  ToJson.default[J, ServerPayload[Person]]
  //  val x = implicitly[ToJson[org.xingyi.script.ServerPayload[one.xingyi.scriptBackend.Person]]]
  //  val y = implicitly[ToServiceResponse]


  //  val newPerson = function[PersonRequest, ServerPayload[Person]]("newPerson") { req => edit(req.name, Person.prototype.copy(name = req.name)) } |+| endpoint[PersonRequest, ServerPayload[Person]]("/person", MatchesServiceRequest.idAtEnd(Method("post")))

  val domainDetails: DomainDetails[Person] = implicitly[DomainDefnToDetails[Person]].apply(new ExampleDomainDefn)

  implicit val domainList = DomainList(domainDetails)
  val javascript = domainDetails.code(Javascript)
  val scala = domainDetails.code(ScalaFull)
  val codeMap = Map(javascript.hash -> javascript.code, scala.hash -> scala.code)

  val allCode = function[CodeRequest, Code[Person]]("code") { codeRequest => Code(domainDetails) } |+| endpoint[CodeRequest, Code[Person]]("/code", MatchesServiceRequest.fixedPath(Method("get")))
  val codeDetails = function[CodeDetailsRequest, CodeDetailsResponse]("codeDetails") {
    codeRequest => CodeDetailsResponse(codeMap.getOrElse(codeRequest.hash, throw new RuntimeException("Cannot find hash. values are" + codeMap.keys)))
  } |+| endpoint[CodeDetailsRequest, CodeDetailsResponse]("/code", MatchesServiceRequest.idAtEnd(Method("get")))

  val newPerson = function[PersonRequest, ServerPayload[Person]]("newPerson") { req => edit(req.name, Person.prototype.copy(name = req.name), req.xingYiHeader) } |+| endpoint[PersonRequest, ServerPayload[Person]]("/person", MatchesServiceRequest.idAtEnd(Method("post")))
  val getPerson = function[PersonRequest, ServerPayload[Person]]("findPerson")(req => ServerPayload(Status(200), people.getOrElse(req.name, throw new RuntimeException("not found")), domainList.accept(req.xingYiHeader))) |+| endpoint[PersonRequest, ServerPayload[Person]]("/person", MatchesServiceRequest.idAtEnd(Method("get")))
  val editPerson = function[EditPersonRequest, ServerPayload[Person]]("editPerson") { req => edit(req.person.name, req.person, req.xingYiHeader) } |+| endpoint[EditPersonRequest, ServerPayload[Person]]("/person", MatchesServiceRequest.idAtEnd(Method("put")))

  //  val getPerson = function[PersonRequest, Person]("findPerson")(req => people.find(_ == req.name).getOrElse(throw new RuntimeException("not found"))) |+| endpoint[PersonRequest, Person]("/person", MatchesServiceRequest.idAtEnd(Method("get")))
  //  ;
  val keepalive: ServiceRequest => M[Option[ServiceResponse]] = function[ServiceRequest, ServiceResponse]("keepalive")(sr => ServiceResponse("Alive")) |+| endpoint[ServiceRequest, ServiceResponse]("/ping", MatchesServiceRequest.fixedPath(Method("get")))


  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(newPerson, getPerson, editPerson, allCode, codeDetails, keepalive)


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
