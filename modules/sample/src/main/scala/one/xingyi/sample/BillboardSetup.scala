package one.xingyi.sample

import one.xingyi.sample.domain.{Promotion, PromotionQuery}
import one.xingyi.tagless.TaglessLanguage
import one.xingyi.sample.domain._
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.functions.MonadCanFail
import one.xingyi.core.http.{Failer, Get, ServiceRequest, ServiceResponse}

import scala.language.{higherKinds, implicitConversions}

//class BillboardSetup[M[_], Fail](interpreter: MicroserviceBuilder[M, Fail] with MicroserviceComposers[M])
class BillboardSetup[ M[_],Wrapper[_, _], Fail](interpreter: TaglessLanguage[M, Wrapper])
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
