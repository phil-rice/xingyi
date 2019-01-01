/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptSharedBackend

import one.xingyi.core.builder.{CopyWithNewId, HasId}
import one.xingyi.core.endpoint.{EndpointKleisli, MatchesServiceRequest}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.logging._
import one.xingyi.core.monad._
import one.xingyi.core.script._
import one.xingyi.core.simpleServer.CheapServer
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

class EntityStore[M[_] : Monad, SharedE, DomainE](implicit changeId: CopyWithNewId[DomainE, String], objectProjection: ObjectProjection[SharedE, DomainE]) {

  var store = Map[String, DomainE]()
  newEntity apply "someName"

  def getEntity: String => M[DomainE] = id => store.getOrElse(id, throw new RuntimeException("could not find name: " + id)).liftM[M]
  def putEntity: (String, DomainE) => M[DomainE] = { (id, entity) => store = store + (id -> entity); entity.liftM }
  def newEntity = { id: String => putEntity(id, changeId(id, objectProjection.prototype)) }

}

case class EntityRequest(id: String, hostAndPost: String)

object EntityRequest {

  import one.xingyi.core.language.AnyLanguage._

  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, EntityRequest] = {
    sr => EntityRequest(Strings.lastSection("/")(sr.path.path), sr.host).liftM[M]
  }

}

case class EntityResponse[T](id: String, entity: T) {
}

object EntityResponse {
  implicit def toServiceResponse[J, Shared, Domain](implicit jsonWriter: JsonWriter[J], projection: ObjectProjection[Shared, Domain]): ToServiceResponse[EntityRequest, ServerPayload[Domain]] =
    cdreq => cdres =>
      ServiceResponse(Status(200), Body(jsonWriter(projection.toJson(cdres.domainObject))), ContentType("text/html"))
}


import one.xingyi.core.language.AnyLanguage._

case class EntityServiceFinderRequest(host: String)

object EntityServiceFinderRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, EntityServiceFinderRequest] = sr => EntityServiceFinderRequest(sr.host).liftM[M]
}

case class EntityServiceFinderResponse(hostAndPort: String, codePattern: String, urlPattern: String, supportedVerbs: List[String])

object EntityServiceFinderResponse extends JsonWriterLanguage {
  implicit def toJson[J: JsonWriter]: ToJsonLib[EntityServiceFinderResponse] = { res =>
    import res._
    def toPath(hash: String) = codePattern.replace("<host>", hostAndPort).replace("<hash>", hash)

    JsonObject(
      "url" -> urlPattern.replace("<host>", hostAndPort), "verbs" -> JsonList(supportedVerbs.map(JsonString.apply)))
  }
}

case class PersonRequest(name: String, host: String, xingYiHeader: Option[String])

object PersonRequest {
  implicit def hasId: HasId[PersonRequest, String] = _.name

  implicit def hasHost: HasHost[PersonRequest] = _.host

  implicit def fromServiceRequest[M[_]](implicit monad: Monad[M]): FromServiceRequest[M, PersonRequest] = { sr =>
    monad.liftM(PersonRequest(Strings.lastSection("/")(sr.uri.path.path), sr.host, sr.header("accept")))
  }

  implicit def toContentType: ToContentType[PersonRequest] = req => req.xingYiHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)
}

case class EditPersonRequest[P](person: P, xingYiHeader: Option[String], host: String) // aha need to be able to make from projection
object EditPersonRequest {
  implicit def hasId[P](implicit hasId: HasId[P, String]): HasId[EditPersonRequest[P], String] = req => hasId(req.person)

  implicit def hasHost[P]: HasHost[EditPersonRequest[P]] = _.host

  implicit def fromServiceRequest[M[_], J: JsonParser, SharedP, DomainP](implicit monad: Monad[M], projection: Projection[SharedP, DomainP]): FromServiceRequest[M, EditPersonRequest[DomainP]] = { sr =>
    val name = Strings.lastButOneSection("/")(sr.uri.path.path)
    val newPerson: DomainP = projection.fromJsonString(sr.body.getOrElse(throw new RuntimeException("cannot create person as body of request empty")).s)
    //    if (name != newPerson.name) throw new RuntimeException("Cannot edit name")
    monad.liftM(EditPersonRequest(newPerson, sr.header("xingyi"), sr.host))
  }

  implicit def toContentType[P]: ToContentType[EditPersonRequest[P]] = req => req.xingYiHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)

}

class EntityCodeMaker[M[_], Fail, SharedE, DomainE](implicit val monad: MonadCanFailWithException[M, Fail], failer: SimpleFailer[Fail],
                                                    domainList: DomainList[DomainE],
                                                    entityPrefixFinder: EntityPrefix[DomainE],
                                                    proof: ProofOfBinding[SharedE, DomainE]) extends LiftFunctionKleisli[M] with MicroserviceComposers[M] with EndpointKleisli[M] {

  val entityPrefix = entityPrefixFinder()

  def hashMap = domainList.domains.map(_.code).
    foldLeft(Map[String, CodeDetailsResponse]()) { (acc, map) => map.foldLeft(acc) { case (acc, (frag, details)) => acc + (details.hash -> CodeDetailsResponse(details.code, frag.mediaType)) } }

  def allCode: CodeRequest => Code[DomainE] = _ => Code(domainList)

  def codeDetails: CodeDetailsRequest => M[CodeDetailsResponse] = cr =>
    hashMap.get(cr.hash).fold(monad.fail[CodeDetailsResponse](failer.simpleNotFound("Cannot find hash. values are" + hashMap.keys.toList.sorted)))(_.liftM[M])

  def allCodeEndpoint[J: JsonWriter] = function(s"$entityPrefix.allCode")(allCode) |+| endpoint[CodeRequest, Code[DomainE]](s"/$entityPrefix/code", MatchesServiceRequest.fixedPath(Method("get")))
  def codeDetailsEndpoint[J: JsonWriter] = codeDetails |+| endpoint[CodeDetailsRequest, CodeDetailsResponse](s"/$entityPrefix/code", MatchesServiceRequest.idAtEnd(Method("get")))

  def endpoints[J: JsonWriter] = List(allCodeEndpoint, codeDetailsEndpoint)
}

case class GetEntityRequest(id: String, host: String, xingYiHeader: Option[String])

object GetEntityRequest {
  implicit def hasId: HasId[GetEntityRequest, String] = _.id

  implicit def hasHost: HasHost[GetEntityRequest] = _.host


  implicit def fromServiceRequest[M[_]](implicit monad: Monad[M]): FromServiceRequest[M, GetEntityRequest] = { sr =>
    monad.liftM(GetEntityRequest(Strings.lastSection("/")(sr.uri.path.path), sr.host, sr.header("accept")))
  }

  implicit def toContentType: ToContentType[GetEntityRequest] = req => req.xingYiHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)
}

case class EditEntityRequest[P](entity: P, xingYiHeader: Option[String], host: String) // aha need to be able to make from projection
object EditEntityRequest {
  implicit def hasId[P](implicit hasId: HasId[P, String]): HasId[EditEntityRequest[P], String] = req => hasId(req.entity)

  implicit def hasHost[P]: HasHost[EditEntityRequest[P]] = _.host

  implicit def fromServiceRequest[M[_], J: JsonParser, SharedP, DomainP](implicit monad: Monad[M], hasId: HasId[DomainP, String], projection: Projection[SharedP, DomainP]): FromServiceRequest[M, EditEntityRequest[DomainP]] = { sr =>
    val name = Strings.lastButOneSection("/")(sr.uri.path.path)
    val newEntity: DomainP = projection.fromJsonString(sr.body.getOrElse(throw new RuntimeException("cannot create as body of request empty")).s)
    if (name != hasId(newEntity)) throw new RuntimeException(s"Cannot edit name. Name is request is $name. Object is $newEntity")
    monad.liftM(EditEntityRequest(newEntity, sr.header("xingyi"), sr.host))
  }

  implicit def toContentType[P]: ToContentType[EditEntityRequest[P]] = req => req.xingYiHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)

}


class EntityMaker[M[_], Fail, SharedE, DomainE: Links](methods: List[Method])
                                                      (implicit val monad: MonadCanFailWithException[M, Fail], failer: SimpleFailer[Fail],
                                                       domainList: DomainList[DomainE],
                                                       entityPrefixFinder: EntityPrefix[DomainE],
                                                       hasId: HasId[DomainE, String],
                                                       proof: ProofOfBinding[SharedE, DomainE]) extends LiftFunctionKleisli[M] with MicroserviceComposers[M] with EndpointKleisli[M] {
  val entityPrefix = entityPrefixFinder()
  import one.xingyi.core.language.MonadLanguage._

  def detailsEndpoint[J: JsonWriter] = function[EntityServiceFinderRequest, EntityServiceFinderResponse](s"$entityPrefix.persondetails")(
    req => EntityServiceFinderResponse(req.host, s"http://${req.host}/code/<hash>", s"http://${req.host}/$entityPrefix/<id>", methods.map(_.toString))
  ) |+| endpoint[EntityServiceFinderRequest, EntityServiceFinderResponse](s"/$entityPrefix", MatchesServiceRequest.fixedPath(Method("get")))

  def makeServerPayload[Req](xingYiHeader: Option[String], dom: DomainE) =
    ServerPayload(Status(200), dom, domainList.accept(xingYiHeader))

  def getEndpoint[J: JsonWriter](fn: String => M[DomainE])(implicit project: ObjectProjection[SharedE, DomainE]): ServiceRequest => M[Option[ServiceResponse]] = {
    req: GetEntityRequest =>
      fn(req.id).map(dom => makeServerPayload(req.xingYiHeader, dom))
  } |+| endpoint[GetEntityRequest, ServerPayload[DomainE]](s"/$entityPrefix", MatchesServiceRequest.idAtEnd(Method("get")))

  def newEntity[J: JsonWriter](fn: String => M[DomainE])(implicit project: ObjectProjection[SharedE, DomainE]) = { req: GetEntityRequest =>
    fn(req.id).map { dom => makeServerPayload(req.xingYiHeader, dom) }
  } |+| endpoint[GetEntityRequest, ServerPayload[DomainE]](s"/$entityPrefix", MatchesServiceRequest.idAtEnd(Method("post")))


  def editEndpoint[J: JsonWriter : JsonParser](fn: (String, DomainE) => M[DomainE])(implicit project: ObjectProjection[SharedE, DomainE], links: Links[DomainE]) = {
    req: EditEntityRequest[DomainE] => fn(hasId(req.entity), req.entity).map(newDom => makeServerPayload(req.xingYiHeader, newDom))
  } |+| endpoint[EditEntityRequest[DomainE], ServerPayload[DomainE]](s"/$entityPrefix", MatchesServiceRequest.idAtEnd(Method("put")))

}
class SharedBackend[M[_] : Async, Fail: Failer, J: JsonParser : JsonWriter, SharedP, DomainP: Links : EntityPrefix : DomainList]
(implicit val monad: MonadCanFailWithException[M, Fail],
 val logReqAndResult: LogRequestAndResult[Fail],
 hasId: HasId[DomainP, String],
 copyWithId: CopyWithNewId[DomainP, String],
 loggingAdapter: LoggingAdapter, projection: ObjectProjection[SharedP, DomainP]) extends CheapServer[M, Fail](9001) {
  import projection.proof

  val personStore = new EntityStore[M, SharedP, DomainP]
  val entityCodeMaker = new EntityCodeMaker[M, Fail, SharedP, DomainP]
  val entityMaker = new EntityMaker[M, Fail, SharedP, DomainP](List(Get, Post, Put))

  val keepalive: ServiceRequest => M[Option[ServiceResponse]] = function[ServiceRequest, ServiceResponse]("keepalive")(sr => ServiceResponse("Alive")) |+| endpoint[ServiceRequest, ServiceResponse]("/ping", MatchesServiceRequest.fixedPath(Method("get")))

  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(
    entityMaker.detailsEndpoint,
    chain(entityCodeMaker.endpoints: _*),
    entityMaker.newEntity(id => copyWithId(id, projection.prototype).liftM),
    entityMaker.getEndpoint(personStore.getEntity),
    entityMaker.editEndpoint(personStore.putEntity),
    keepalive)

}
