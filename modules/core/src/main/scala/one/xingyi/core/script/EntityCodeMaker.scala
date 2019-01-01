package one.xingyi.core.script
import one.xingyi.core.builder.HasId
import one.xingyi.core.endpoint.{EndpointKleisli, MatchesServiceRequest}
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter, ObjectProjection, ProofOfBinding}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.monad.{LiftFunctionKleisli, MonadCanFailWithException}

import scala.language.higherKinds



class EntityCodeMaker[M[_], Fail, SharedE, DomainE](implicit val monad: MonadCanFailWithException[M, Fail], failer: SimpleFailer[Fail],
                                                    domainList: DomainList[SharedE, DomainE],
                                                    entityPrefixFinder: EntityPrefix[DomainE],
                                                    proof: ProofOfBinding[SharedE, DomainE]) extends LiftFunctionKleisli[M] with MicroserviceComposers[M] with EndpointKleisli[M] {

  val entityPrefix = entityPrefixFinder()

  def hashMap = domainList.domains.map(_.code).
    foldLeft(Map[String, CodeDetailsResponse]()) { (acc, map) => map.foldLeft(acc) { case (m, (frag, details)) => m + (details.hash -> CodeDetailsResponse(details.code, frag.mediaType)) } }

  def allCode: CodeRequest => CodeResponse[SharedE, DomainE] = _ => CodeResponse(domainList)

  def codeDetails: CodeDetailsRequest => M[CodeDetailsResponse] = cr =>
    hashMap.get(cr.hash).fold(monad.fail[CodeDetailsResponse](failer.simpleNotFound("Cannot find hash. values are" + hashMap.keys.toList.sorted)))(_.liftM[M])

  def allCodeEndpoint[J: JsonWriter] = function(s"$entityPrefix.allCode")(allCode) |+| endpoint[CodeRequest, CodeResponse[SharedE, DomainE]](s"/$entityPrefix/code", MatchesServiceRequest.fixedPath(Method("get")))
  def codeDetailsEndpoint[J: JsonWriter] = codeDetails |+| endpoint[CodeDetailsRequest, CodeDetailsResponse](s"/$entityPrefix/code", MatchesServiceRequest.idAtEnd(Method("get")))

  def endpoints[J: JsonWriter] = List(allCodeEndpoint, codeDetailsEndpoint)
}


class EntityMaker[M[_], Fail, SharedE, DomainE: Links](methods: List[Method])
                                                      (implicit val monad: MonadCanFailWithException[M, Fail], failer: SimpleFailer[Fail],
                                                       domainList: DomainList[SharedE, DomainE],
                                                       entityPrefixFinder: EntityPrefix[DomainE],
                                                       hasId: HasId[DomainE, String],
                                                       proof: ProofOfBinding[SharedE, DomainE]) extends LiftFunctionKleisli[M] with MicroserviceComposers[M] with EndpointKleisli[M] {
  val entityPrefix = entityPrefixFinder()
  import one.xingyi.core.language.MonadLanguage._

  val details = function[EntityServiceFinderRequest, EntityServiceFinderResponse](s"$entityPrefix.persondetails")(
    req => EntityServiceFinderResponse(req.host, s"http://${req.host}/code/<hash>", s"http://${req.host}/$entityPrefix/<id>", methods.map(_.toString), domainList)
  )
  def detailsEndpoint[J: JsonWriter] =
    details |+| endpoint[EntityServiceFinderRequest, EntityServiceFinderResponse](s"/$entityPrefix", MatchesServiceRequest.fixedPath(Method("get")))


  def makeServerPayload[Req](xingYiHeader: Option[String], dom: DomainE) =
    ServerPayload(Status(200), dom, domainList.accept(xingYiHeader))

  def getFn(fn: String => M[DomainE]) = { req: GetEntityRequest => fn(req.id).map(dom => makeServerPayload(req.xingYiHeader, dom)) }
  def getEndpoint[J: JsonWriter](fn: String => M[DomainE])(implicit project: ObjectProjection[SharedE, DomainE]): ServiceRequest => M[Option[ServiceResponse]] =
    getFn(fn) |+| endpoint[GetEntityRequest, ServerPayload[SharedE, DomainE]](s"/$entityPrefix", MatchesServiceRequest.idAtEnd(Method("get")))

  def newFn(fn: String => M[DomainE]) = { req: GetEntityRequest => fn(req.id).map { dom => makeServerPayload(req.xingYiHeader, dom) } }
  def newEntity[J: JsonWriter](fn: String => M[DomainE])(implicit project: ObjectProjection[SharedE, DomainE]) =
    newFn(fn) |+| endpoint[GetEntityRequest, ServerPayload[SharedE, DomainE]](s"/$entityPrefix", MatchesServiceRequest.idAtEnd(Method("post")))

  def editFn(fn: (String, DomainE) => M[DomainE]) = { req: EditEntityRequest[DomainE] => fn(hasId(req.entity), req.entity).map(newDom => makeServerPayload(req.xingYiHeader, newDom)) }
  def editEndpoint[J: JsonWriter : JsonParser](fn: (String, DomainE) => M[DomainE])(implicit project: ObjectProjection[SharedE, DomainE], links: Links[DomainE]) =
    editFn(fn) |+| endpoint[EditEntityRequest[DomainE], ServerPayload[SharedE, DomainE]](s"/$entityPrefix", MatchesServiceRequest.idAtEnd(Method("put")))

}

