package org.validoc.sample.domain

import org.validoc.playJson.PlayJsonDomainObject
import org.validoc.utils.metrics.{MetricValue, ReportData}
import play.api.libs.json.{Json, OFormat}

import scala.util.Try
//needs to be here import io.circe.generic.auto._
import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, Id, UnitId}
import org.validoc.utils.http._

import scala.language.implicitConversions

trait MostPopularQuery

object MostPopularQuery extends MostPopularQuery {

  implicit object CachableKeyForMostPopularQuery extends CachableKey[MostPopularQuery] {
    override def id(req: MostPopularQuery): Id = UnitId

    override def bypassCache(req: MostPopularQuery): Boolean = false
  }

  implicit def toRequestForMostPopularQuery(req: MostPopularQuery) = ServiceRequest(Get, Uri("http://someUri"))


  implicit def fromServiceRequestForMostPopularQuery(v1: ServiceRequest) = MostPopularQuery


  implicit def fromHomePageQuery(h: HomePageQuery) = MostPopularQuery

}


case class MostPopular(programmeIds: Seq[ProgrammeId])


object MostPopular extends PlayJsonDomainObject[MostPopular] {

  implicit object ReportDataForMostPopular extends ReportData[MostPopular] {
    override def apply(v1: String, v2: Try[MostPopular], v3: Long): Map[String, MetricValue] = Map()
  }
  implicit val modelFormat: OFormat[MostPopular] = Json.format[MostPopular]

  implicit object HasChildrenForMostPopular extends HasChildren[MostPopular, ProgrammeId] {
    override def apply(p: MostPopular): Seq[ProgrammeId] = p.programmeIds
  }

}

case class EnrichedMostPopular(programmes: Seq[Programme])

object EnrichedMostPopular extends PlayJsonDomainObject[EnrichedMostPopular] {
  implicit val modelFormat: OFormat[EnrichedMostPopular] = Json.format[EnrichedMostPopular]

  implicit object EnricherForMostPopular extends Enricher[EnrichedMostPopular, MostPopular, Programme] {
    override def apply(p: MostPopular)(children: Seq[Programme]): EnrichedMostPopular =
      EnrichedMostPopular(children)
  }

  def apply(p: MostPopular, children: Seq[Programme]): EnrichedMostPopular = EnrichedMostPopular(children)

}