package org.validoc.sample.domain

import org.validoc.utils.cache.Cachable
import org.validoc.utils.domain.{BypassCache, DomainCompanionObject, DomainCompanionQuery}
import org.validoc.utils.tagless.{Enricher, HasChildren}
//needs to be here import io.circe.generic.auto._
import org.validoc.utils.http._

import scala.language.implicitConversions

case class MostPopularQuery(bypassCache: Boolean) extends BypassCache

object MostPopularQuery extends DomainCompanionQuery[MostPopularQuery] {

  implicit object CachableKeyForMostPopularQuery extends Cachable[MostPopularQuery] {
    override def apply(v1: MostPopularQuery) = ()
  }


  implicit def toRequestForMostPopularQuery = new ToServiceRequest[MostPopularQuery] {
    override def apply(v1: MostPopularQuery): ServiceRequest = ServiceRequest(Get, Uri("/mostpopular"))
  }


  implicit object fromServiceRequestForMostPopularQuery extends FromServiceRequest[MostPopularQuery] {
    override def apply(v1: ServiceRequest): MostPopularQuery = MostPopularQuery(false)
  }

  implicit def fromHomePageQuery(h: HomePageQuery) = MostPopularQuery(h.bypassCache)

}


case class MostPopular(programmeIds: Seq[ProgrammeId])


object MostPopular extends DomainCompanionObject[MostPopularQuery, MostPopular] {

  implicit object HasChildrenForMostPopular extends HasChildren[MostPopular, ProgrammeId] {
    override def apply(p: MostPopular): Seq[ProgrammeId] = p.programmeIds
  }

}

case class EnrichedMostPopular(programmes: Seq[Programme])

object EnrichedMostPopular extends DomainCompanionObject[MostPopularQuery, EnrichedMostPopular] {
  implicit object EnricherFor extends Enricher[MostPopularQuery, MostPopular, ProgrammeId, Programme, EnrichedMostPopular] {
    override def apply(v1: MostPopularQuery, v2: MostPopular, v3: Seq[(ProgrammeId, Programme)]) = EnrichedMostPopular(v3.map(_._2))
  }
}