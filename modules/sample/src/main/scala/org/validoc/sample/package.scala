package org.validoc

import org.validoc.sample.domain._
import scala.language.higherKinds
import scala.language.postfixOps


package object sample {

  type Service[M[_], Req, Res] = Req => M[Res]

  type MostPopularService[M[_]] = Service[M, Unit, MostPopular]
  type PromotionService[M[_]] = Service[M, Unit, List[Promotion]]

  type ProductionService[M[_]] = Service[M, ProductionId, Production]
  type ProgrammeService[M[_]] = Service[M, ProgrammeId, Programme]

  type EnrichPromotionService[M[_]] = Service[M, Unit, List[EnrichedPromotion]]
  type EnrichMostPopularService[M[_]] = Service[M, Unit, EnrichedMostPopular]
}
