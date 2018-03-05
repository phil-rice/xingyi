package org.validoc.sample.domain

import org.validoc.utils.cache.{CachableKey, ObjectId}
import org.validoc.utils.domain.{BypassCache, DomainCompanionObject, DomainCompanionQuery}
import org.validoc.utils.functions.Monad
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.language.Language._
import org.validoc.utils.strings.Strings

import scala.language.{higherKinds, implicitConversions}

case class ProductionId(id: String, bypassCache: Boolean) extends BypassCache

object ProductionId extends DomainCompanionQuery[ProductionId] {

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

object Production extends DomainCompanionObject[ProductionId, Production] {

  implicit object ToJsonForPromotion extends ToJson[Production] {
    override def apply(v1: Production) = s"""{productionInfo: "${v1.info}"}"""
  }
}