package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.tagless.TaglessLanguage
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http.{Failer, Get, ServiceRequest, ServiceResponse}

import scala.language.{higherKinds, implicitConversions}

class VogueSetup[ Wrapper[_, _], M[_], Fail](interpreter: TaglessLanguage[ Wrapper, M])
                                                                  (implicit
                                                                   monadCanFail: MonadCanFail[M, Fail],
                                                                   failer: Failer[Fail],
                                                                   jsonBundle: JsonBundle
                                                                  ) extends PromotionServiceNames {

  import interpreter._

  implicit def toProgrammeId(id: Int) = ProgrammeId(id.toString, false)
  val mostpopular = function[MostPopularQuery, MostPopular]("mostpopular")(query => MostPopular(List(1, 2, 3)))
  val mostpopularEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = mostpopular |+| endpoint[MostPopularQuery, MostPopular]("/mostpopular", MatchesServiceRequest.fixedPath(Get))

  val microservice = chain(mostpopularEndpoint)
}
