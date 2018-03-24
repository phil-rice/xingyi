package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.tagless.TaglessLanguage
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http.{Failer, Get, ServiceRequest, ServiceResponse}

import scala.language.{higherKinds, implicitConversions}

//class BillboardSetup[M[_], Fail](interpreter: MicroserviceBuilder[M, Fail] with MicroserviceComposers[M])
class BillboardSetup[Wrapper[_, _], M[_], Fail](interpreter: TaglessLanguage[Wrapper, M])
                                               (implicit
                                                monadCanFail: MonadCanFail[M, Fail],
                                                failer: Failer[Fail],
                                                jsonBundle: JsonBundle
                                               ) extends PromotionServiceNames {

  import interpreter._

  implicit def toProductionId(id: Int) = domain.ProductionId(id.toString, false)
  val billboard = function[PromotionQuery, Promotion]("promotions")(query => Promotion(List(1, 2, 3)))
  val billboardEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = billboard |+| endpoint[PromotionQuery, Promotion]("/billboard", MatchesServiceRequest.fixedPath(Get))

  val microservice = chain(billboardEndpoint)
}
