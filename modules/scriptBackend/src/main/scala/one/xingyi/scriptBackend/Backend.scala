/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend

import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.logging._
import one.xingyi.core.monad.{Async, IdentityMonad, Monad, MonadCanFailWithException}
import one.xingyi.core.script._
import one.xingyi.core.simpleServer.CheapServer
import one.xingyi.core.strings.Strings
import one.xingyi.scriptShared.IPerson
import org.json4s.JValue

import scala.language.higherKinds

trait PersonStore {
  val tel = Telephone("someNumber")
  val address1 = Address("line1a", "line1b", "pc1")
  val address2 = Address("line2a", "line2b", "pc2")
  val person = Person("someName", List(address1, address2), tel)
  var people = Map[String, Person]("someName" -> person)

  def save(id: String, person: Person) = people = people + (id -> person)


}

case class EntityRequest(id: String)
object EntityRequest {
  import one.xingyi.core.language.AnyLanguage._
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, EntityRequest] = {
    sr => EntityRequest(Strings.lastSection("/")(sr.path.path)).liftM[M]
  }

}
case class EntityResponse[T](id: String, entity: T) {
}

object EntityResponse {

  implicit def toServiceResponse[J, Shared, Domain](implicit jsonWriter: JsonWriter[J], projection: ObjectProjection[Shared, Domain]): ToServiceResponse[EntityRequest, ServerPayload[Domain]] =
    cdreq => cdres =>
      ServiceResponse(Status(200), Body(jsonWriter(projection.toJson(cdres.domainObject))), ContentType("text/html"))


}

class NewBackend extends PersonStore {


  case class entity[Shared, Domain](implicit projection: ObjectProjection[Shared, Domain]) {
    case class atUrl(urlPattern: String) {
      case class saveUsing(saveFn: (String, Person) => Unit) {
        case class loadUsing(loadFn: String => Person) {


        }
      }
    }
  }

  entity[IPerson, Person] atUrl "/person/<id>" saveUsing save loadUsing people.apply
}

import one.xingyi.core.language.AnyLanguage._
case class PersonServiceFinderRequest()
object PersonServiceFinderRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, PersonServiceFinderRequest] = sr => PersonServiceFinderRequest().liftM[M]
}
case class PersonServiceFinderResponse(javascriptUrl: String, scalaUrl: String, urlPattern: String, supportedVerbs: List[String])
object PersonServiceFinderResponse extends JsonWriterLanguage {
  implicit def toJson[J: JsonWriter]: ToJsonLib[PersonServiceFinderResponse] = { res =>
    import res._
    JsonObject(
      "url" -> urlPattern, "verbs" -> JsonList(supportedVerbs.map(JsonString.apply)),
      "code" -> JsonObject("javascript" -> javascriptUrl, "scala" -> scalaUrl))
  }
}

class Backend[M[_] : Async, Fail: Failer : LogRequestAndResult, J: JsonParser : JsonWriter]
(implicit val monad: MonadCanFailWithException[M, Fail], val logReqAndResult: LogRequestAndResult[Fail], loggingAdapter: LoggingAdapter)
  extends CheapServer[M, Fail](9001) with PersonStore {
  def edit(name: String, person: Person, xingYiHeader: Option[String])(implicit domainList: DomainList[Person]) = {
    people = people + (name -> person)
    println(s"changing $name people now $people header was $xingYiHeader")
    ServerPayload(Status(200), person, domainList.accept(xingYiHeader))
  }

  val domainDetails: DomainDetails[Person] = implicitly[DomainDefnToDetails[Person]].apply(new ExampleDomainDefn)

  implicit val domainList = DomainList(domainDetails)
  val javascript = domainDetails.code(Javascript)
  val scala = domainDetails.code(ScalaCode)
  val codeMap = Map(javascript.hash -> javascript.code, scala.hash -> scala.code)

  val allCode = function[CodeRequest, Code[Person]]("code") { codeRequest => Code(domainDetails) } |+| endpoint[CodeRequest, Code[Person]]("/code", MatchesServiceRequest.fixedPath(Method("get")))
  val codeDetails = function[CodeDetailsRequest, CodeDetailsResponse]("codeDetails") {
    codeRequest => CodeDetailsResponse(codeMap.getOrElse(codeRequest.hash, throw new RuntimeException("Cannot find hash. values are" + codeMap.keys)))
  } |+| endpoint[CodeDetailsRequest, CodeDetailsResponse]("/code", MatchesServiceRequest.idAtEnd(Method("get")))

  //  ServerPayload.toServerResponse[J, PersonRequest, IPerson, Person]
  //  val x = implicitly[ToJson[one.xingyi.core.script.ServerPayload[one.xingyi.scriptBackend.Person]]]
  //  val y = implicitly[ToServiceResponse[Person]]

  val personDetails = function[PersonServiceFinderRequest, PersonServiceFinderResponse]("persondetails")(
    _ => PersonServiceFinderResponse(s"/code/${javascript.hash}", s"/code/${scala.hash}", "/person/<id>", List("put", "post", "get"))
  ) |+| endpoint[PersonServiceFinderRequest, PersonServiceFinderResponse]("/person", MatchesServiceRequest.fixedPath(Method("get")))

  val newPerson = function[PersonRequest, ServerPayload[Person]]("newPerson") { req => edit(req.name, Person.prototype.copy(name = req.name), req.xingYiHeader) } |+| endpoint[PersonRequest, ServerPayload[Person]]("/person", MatchesServiceRequest.idAtEnd(Method("post")))
  val getPerson = function[PersonRequest, ServerPayload[Person]]("findPerson")(req => ServerPayload(Status(200), people.getOrElse(req.name, throw new RuntimeException("not found")), domainList.accept(req.xingYiHeader))) |+| endpoint[PersonRequest, ServerPayload[Person]]("/person", MatchesServiceRequest.idAtEnd(Method("get")))
  val editPerson = function[EditPersonRequest, ServerPayload[Person]]("editPerson") { req => edit(req.person.name, req.person, req.xingYiHeader) } |+| endpoint[EditPersonRequest, ServerPayload[Person]]("/person", MatchesServiceRequest.idAtEnd(Method("put")))

  //  val getPerson = function[PersonRequest, Person]("findPerson")(req => people.find(_ == req.name).getOrElse(throw new RuntimeException("not found"))) |+| endpoint[PersonRequest, Person]("/person", MatchesServiceRequest.idAtEnd(Method("get")))
  //  ;
  val keepalive: ServiceRequest => M[Option[ServiceResponse]] = function[ServiceRequest, ServiceResponse]("keepalive")(sr => ServiceResponse("Alive")) |+| endpoint[ServiceRequest, ServiceResponse]("/ping", MatchesServiceRequest.fixedPath(Method("get")))


  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(personDetails, newPerson, getPerson, editPerson, allCode, codeDetails, keepalive)


}
import one.xingyi.core.http.Failer.failerForThrowable
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
object Backend extends App {
  implicit val logger: LoggingAdapter = PrintlnLoggingAdapter
  import SimpleLogRequestAndResult._

  val backend = new Backend[IdentityMonad, Throwable, JValue]

  println("running")
  backend.start
}
