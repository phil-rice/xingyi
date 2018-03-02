package org.validoc.sample.domain

import org.validoc.utils.cache.Cachable
import org.validoc.utils.domain.{BypassCache, DomainCompanionObject, DomainCompanionQuery}
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}

import scala.language.implicitConversions

case class ProductionId(id: String, bypassCache: Boolean) extends BypassCache

object ProductionId extends DomainCompanionQuery[ProductionId] {

  implicit object toRequestForProductionId extends ToServiceRequest[ProductionId] {
    override def apply(v1: ProductionId): ServiceRequest = ServiceRequest(Get, Uri(s"/production/${v1.id}"))
  }
  implicit object CachableKeyForProductionId extends Cachable[ProductionId] {
    override def apply(v1: ProductionId) = v1.id
  }
}

case class Production(info: String)

object Production extends DomainCompanionObject[ProductionId, Production]