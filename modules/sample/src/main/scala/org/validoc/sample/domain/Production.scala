package org.validoc.sample.domain

import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, StringId}
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}
import org.validoc.utils.metrics.{MetricValue, ReportData}

import scala.language.implicitConversions
import scala.util.Try

case class ProductionId(id: String) extends AnyVal

object ProductionId extends DomainCompanionObject[ProductionId] {

  implicit object ToServiceRequestForProductionId extends ToServiceRequest[ProductionId] {
    override def apply(req: ProductionId): ServiceRequest = ServiceRequest(Get, Uri(s"someId/${req.id}"))
  }

  implicit object CachableKeyForProductionId extends CachableKey[ProductionId] {
    override def id(req: ProductionId): Id = StringId(req.id)

    override def bypassCache(req: ProductionId): Boolean = false
  }

}

case class Production(id: ProductionId, info: String)

object Production extends DomainCompanionObject[Production] {

  implicit object CachableResultForProduction extends CachableResultUsingSucesses[Production]

  implicit object ReportDataForProgramme extends ReportData[Production] {
    override def apply(v1: String, v2: Try[Production], v3: Long): Map[String, MetricValue] = Map()
  }

}