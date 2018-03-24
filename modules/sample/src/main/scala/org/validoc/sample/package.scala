package org.validoc

import org.validoc.sample.domain._
import org.validoc.utils.http.ServiceResponse
import org.validoc.utils.profiling.{DontProfile, ProfileAs, ProfileAsFail, ProfileAsSuccess}

import scala.language.{higherKinds, postfixOps}
import scala.util.{Failure, Success, Try}


package object sample {

  type Service[M[_], Req, Res] = Req => M[Res]

  type MostPopularService[M[_]] = Service[M, Unit, MostPopular]
  type PromotionService[M[_]] = Service[M, Unit, List[Promotion]]

  type ProductionService[M[_]] = Service[M, ProductionId, Production]
  type ProgrammeService[M[_]] = Service[M, ProgrammeId, Programme]

  type EnrichPromotionService[M[_]] = Service[M, Unit, List[EnrichedPromotion]]
  type EnrichMostPopularService[M[_]] = Service[M, Unit, EnrichedMostPopular]



}
