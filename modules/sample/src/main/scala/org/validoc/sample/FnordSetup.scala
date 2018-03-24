package org.validoc.sample

import org.validoc.sample.domain.{Production, ProductionId, Programme, ProgrammeId}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http.{Failer, Get, ServiceRequest, ServiceResponse}
import org.validoc.tagless.TaglessLanguage

import scala.language.higherKinds

class FnordSetup[Wrapper[_, _], M[_], Fail](interpreter: TaglessLanguage[Wrapper, M])
                                           (implicit
                                            monadCanFail: MonadCanFail[M, Fail],
                                            failer: Failer[Fail],
                                            jsonBundle: JsonBundle
                                           ) extends PromotionServiceNames {

  import interpreter._

  val production = function[ProductionId, Production]("production")(id => Production(s"from ${id.id}"))
  val program = function[ProgrammeId, Programme]("programme")(id => Programme(s"from ${id.id}"))

  val x = ProgrammeId.fromServiceRequest[M]
  val programmeEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = program |+| endpoint[ProgrammeId, Programme]("/programme", MatchesServiceRequest.idAtEnd(Get))
  val productionEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = production |+| endpoint[ProductionId, Production]("/production", MatchesServiceRequest.idAtEnd(Get))

  val microservice = chain(productionEndpoint, productionEndpoint)
}
