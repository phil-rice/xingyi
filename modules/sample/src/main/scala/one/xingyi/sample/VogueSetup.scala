package one.xingyi.sample

import one.xingyi.sample.domain.{MostPopular, MostPopularQuery, ProgrammeId}
import one.xingyi.tagless.TaglessLanguage
import one.xingyi.sample.domain._
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http.{Failer, Get, ServiceRequest, ServiceResponse}
import one.xingyi.core.monad.MonadCanFail

import scala.language.{higherKinds, implicitConversions}

class VogueSetup[  M[_],Wrapper[_, _], Fail](interpreter: TaglessLanguage[ M, Wrapper])
                                                                  (implicit
                                                                   monadCanFail: MonadCanFail[M, Fail],
                                                                   failer: Failer[Fail]
                                                                  ) extends PromotionServiceNames {

  import interpreter._

  implicit def toProgrammeId(id: Int) = ProgrammeId(id.toString, false)
  val mostpopular = function[MostPopularQuery, MostPopular]("mostpopular")(query => domain.MostPopular(List(1, 2, 3)))
  val mostpopularEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = mostpopular |+| endpoint[MostPopularQuery, MostPopular]("/mostpopular", MatchesServiceRequest.fixedPath(Get))

  val microservice = chain(mostpopularEndpoint)
}
