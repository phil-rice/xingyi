package one.xingyi.sample

import one.xingyi.sample.domain.{Production, ProductionId, Programme, ProgrammeId}
import one.xingyi.tagless.TaglessLanguage
import one.xingyi.sample.domain.{ProductionId, Programme, ProgrammeId}
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.functions.MonadCanFail
import one.xingyi.core.http.{Failer, Get, ServiceRequest, ServiceResponse}

import scala.language.higherKinds

class FnordSetup[ M[_],Wrapper[_, _], Fail](interpreter: TaglessLanguage[M, Wrapper])
                                           (implicit
                                            monadCanFail: MonadCanFail[M, Fail],
                                            failer: Failer[Fail]
                                           ) extends PromotionServiceNames {

  import interpreter._

  val production = function[ProductionId, Production]("production")(id => Production(s"from ${id.id}"))
  val program = function[ProgrammeId, Programme]("programme")(id => Programme(s"from ${id.id}"))

  val x = ProgrammeId.fromServiceRequest[M]
  val programmeEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = program |+| endpoint[ProgrammeId, Programme]("/programme", MatchesServiceRequest.idAtEnd(Get))
  val productionEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = production |+| endpoint[ProductionId, Production]("/production", MatchesServiceRequest.idAtEnd(Get))

  val microservice = chain(productionEndpoint, productionEndpoint)
}
