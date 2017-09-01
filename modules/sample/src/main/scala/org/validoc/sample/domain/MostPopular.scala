package org.validoc.sample.domain

import org.validoc.utils.endpoint.{DebugBasePath, SamplePathOps}
import org.validoc.utils.metrics.{MetricValue, ReportData}

import scala.util.Try
//needs to be here import io.circe.generic.auto._
import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, Id, UnitId}
import org.validoc.utils.http._

import scala.language.implicitConversions

trait MostPopularQuery

object MostPopularQuery extends DomainCompanionObject[MostPopularQuery] with MostPopularQuery {

  implicit object SamplePathOpsForMostPopularQuery extends SamplePathOps[MostPopularQuery] {
    override def samplePath(serviceId: Int)(implicit debugBasePath: DebugBasePath): String = debugBasePath.withService(serviceId)
  }

  implicit object CachableKeyForMostPopularQuery extends CachableKey[MostPopularQuery] {
    override def id(req: MostPopularQuery): Id = UnitId

    override def bypassCache(req: MostPopularQuery): Boolean = false
  }

  implicit def toRequestForMostPopularQuery = new ToServiceRequest[MostPopularQuery] {
    override def apply(v1: MostPopularQuery): ServiceRequest = ServiceRequest(Get, Uri("/mostpopular"))
  }


  implicit object fromServiceRequestForMostPopularQuery extends FromServiceRequest[MostPopularQuery] {
    override def apply(v1: ServiceRequest): MostPopularQuery = MostPopularQuery
  }


  implicit def fromHomePageQuery(h: HomePageQuery) = MostPopularQuery

}


case class MostPopular(programmeIds: Seq[ProgrammeId])


object MostPopular extends DomainCompanionObject[MostPopular] {

  implicit object ReportDataForMostPopular extends ReportData[MostPopular] {
    override def apply(v1: String, v2: Try[MostPopular], v3: Long): Map[String, MetricValue] = Map()
  }

  implicit object HasChildrenForMostPopular extends HasChildren[MostPopular, ProgrammeId] {
    override def apply(p: MostPopular): Seq[ProgrammeId] = p.programmeIds
  }

}

case class EnrichedMostPopular(programmes: Seq[Programme])

object EnrichedMostPopular extends DomainCompanionObject[EnrichedMostPopular] {

  implicit object EnricherForMostPopular extends Enricher[EnrichedMostPopular, MostPopular, Programme] {
    override def apply(p: MostPopular, children: Seq[Programme]): EnrichedMostPopular =
      EnrichedMostPopular(children)
  }

  def apply(p: MostPopular, children: Seq[Programme]): EnrichedMostPopular = EnrichedMostPopular(children)

}