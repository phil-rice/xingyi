package org.validoc.sample.domain

import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, StringId}
import org.validoc.utils.endpoint.{DebugBasePath, DebugEndPointReqOps}
import org.validoc.utils.http._
import org.validoc.utils.metrics.{MetricValue, ReportData}
import org.validoc.utils.strings.Strings

import scala.language.implicitConversions
import scala.util.Try

case class ProductionId(id: String) extends AnyVal

object ProductionId extends DomainCompanionObject[ProductionId] {

  implicit object DebugEndPointReqOpsForProductionId extends DebugEndPointReqOps[ProductionId] {

    override def apply(v1: ServiceRequest): ProductionId = ProductionId(Strings.lastSection("/")(v1.uri.path.path))

    override def samplePath(serviceId: Int)(implicit debugBasePath: DebugBasePath): String = debugBasePath.withServiceAndEntityAndId(serviceId, "productionId", "someProductionId")
  }

  implicit object ToServiceRequestForProductionId extends ToServiceRequest[ProductionId] {
    override def apply(req: ProductionId): ServiceRequest = ServiceRequest(Get, Uri(s"/production/${req.id}"))
  }

  implicit object CachableKeyForProductionId extends CachableKey[ProductionId] {
    override def id(req: ProductionId): Id = StringId(req.id)

    override def bypassCache(req: ProductionId): Boolean = false
  }

}

case class Production(info: String)

object Production extends DomainCompanionObject[Production] {

  implicit object CachableResultForProduction extends CachableResultUsingSucesses[Production]

  implicit object ReportDataForProgramme extends ReportData[Production] {
    override def apply(v1: String, v2: Try[Production], v3: Long): Map[String, MetricValue] = Map()
  }

}