package one.xingyi.sample.domain

import one.xingyi.utils.cache.{CachableKey, ObjectId}
import one.xingyi.utils.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.utils.functions.Monad
import one.xingyi.utils.http._
import one.xingyi.utils.json.ToJson
import one.xingyi.utils.language.Language._
import one.xingyi.utils.strings.Strings

import scala.language.{higherKinds, implicitConversions}

case class ProductionId(id: String, bypassCache: Boolean) extends BypassCache

object ProductionId extends DomainRequestCompanionQuery[ProductionId] {

  implicit object toRequestForProductionId extends ToServiceRequest[ProductionId] {
    override def apply(v1: ProductionId): ServiceRequest = ServiceRequest(Get, Uri(s"/production/${v1.id}"))
  }
  implicit object CachableKeyForProductionId extends CachableKey[ProductionId] {
    //    override def apply(v1: ProductionId) = v1.id
    override def id(req: ProductionId) = ObjectId(req)
    override def bypassCache(req: ProductionId) = false
  }
  implicit def fromServiceRequest[M[_] : Monad] = new FromServiceRequest[M, ProductionId] {
    override def apply(v1: ServiceRequest) = ProductionId(Strings.lastSection("/")(v1.body.map(_.s).getOrElse("")), false).liftM
  }

}

case class Production(info: String)

object Production extends DomainResponseCompanionObject[ProductionId, Production] {

  implicit object ToJsonForPromotion extends ToJson[Production] {
    override def apply(v1: Production) = s"""{productionInfo: "${v1.info}"}"""
  }
}