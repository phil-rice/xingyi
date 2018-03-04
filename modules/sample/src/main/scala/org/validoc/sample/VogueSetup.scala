package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http.{Failer, Get}
import org.validoc.utils.tagless.TaglessLanguage
import org.validoc.utils._
import scala.language.higherKinds
import scala.language.implicitConversions

class VogueSetup[EndpointWrapper[_, _], Wrapper[_, _], M[_], Fail](interpreter: TaglessLanguage[EndpointWrapper, Wrapper, M, Fail])
                                                                  (implicit
                                                                   monadCanFail: MonadCanFail[M, Fail],
                                                                   failer: Failer[Fail],
                                                                   jsonBundle: JsonBundle
                                                                  ) extends PromotionServiceNames {

  import interpreter._

  implicit def toProgrammeId(id: Int) = ProgrammeId(id.toString, false)
  val mostpopular = function[MostPopularQuery, MostPopular]("production")(query => MostPopular(List(1, 2, 3)))
  val mostpopularEndpoint = mostpopular |++| endpoint[MostPopularQuery, MostPopular]("/mostpopular", MatchesServiceRequest.idAtEnd(Get))

  val microservice = chain(mostpopularEndpoint)
}
