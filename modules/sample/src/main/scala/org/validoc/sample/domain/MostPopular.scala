package org.validoc.sample.domain

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.cache.{CachableKey, UnitId}
import org.validoc.utils.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import org.validoc.utils.functions.Liftable
import org.validoc.utils.json.ToJson
//needs to be here import io.circe.generic.auto._
import org.validoc.utils.http._
import org.validoc.utils.language.Language._

import scala.language.{higherKinds, implicitConversions}

case class MostPopularQuery(bypassCache: Boolean) extends BypassCache

object MostPopularQuery extends DomainRequestCompanionQuery[MostPopularQuery] {

  implicit object CachableKeyForMostPopularQuery extends CachableKey[MostPopularQuery] {
//    override def apply(v1: MostPopularQuery) = ()
    override def id(req: MostPopularQuery) = UnitId
    override def bypassCache(req: MostPopularQuery) = req.bypassCache
  }


  implicit def toRequestForMostPopularQuery = new ToServiceRequest[MostPopularQuery] {
    override def apply(v1: MostPopularQuery): ServiceRequest = ServiceRequest(Get, Uri("/mostpopular"))
  }


  implicit def fromServiceRequest[M[_] : Liftable] = new fromServiceRequestForMostPopularQuery[M]

  class fromServiceRequestForMostPopularQuery[M[_] : Liftable] extends FromServiceRequest[M, MostPopularQuery] {
    override def apply(v1: ServiceRequest) = MostPopularQuery(false).liftM
  }

  implicit def fromHomePageQuery(h: HomePageQuery) = MostPopularQuery(h.bypassCache)

}


case class MostPopular(programmeIds: Seq[ProgrammeId])


object MostPopular extends DomainResponseCompanionObject[MostPopularQuery, MostPopular] {

  implicit object HasChildrenForMostPopular extends HasChildren[MostPopular, ProgrammeId] {
    override def apply(p: MostPopular): Seq[ProgrammeId] = p.programmeIds
  }
  implicit object ToJsonForMostPopular extends ToJson[MostPopular] {
    override def apply(v1: MostPopular) = s"""{id: [${v1.programmeIds.map(id => s""""$id"""").mkString(",")}]"""
  }

}

case class EnrichedMostPopular(programmes: Seq[Programme])

object EnrichedMostPopular extends DomainResponseCompanionObject[MostPopularQuery, EnrichedMostPopular] {
  implicit object EnricherFor extends Enricher[MostPopularQuery, MostPopular, ProgrammeId, Programme, EnrichedMostPopular] {
    override def apply(v1: MostPopularQuery, v2: MostPopular, v3: Seq[(ProgrammeId, Programme)]) = EnrichedMostPopular(v3.map(_._2))
  }
}