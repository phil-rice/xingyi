package org.validoc.domain


import org.validoc.utils.{FromServiceRequest, ToServiceRequest}
import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, UnitId}
import org.validoc.utils.http._
import org.validoc.utils.parser.ParserFinder

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

object MostPopular {

  implicit object HasChildrenForMostPopular extends HasChildren[MostPopular, ProgrammeId] {
    override def apply(p: MostPopular): Seq[ProgrammeId] = p.programmeIds
  }

  implicit object CachableResultForMostPopular extends CachableResultUsingSucesses[MostPopular]

  implicit val parserFinderForMostPopular = ParserFinder.always(_ => MostPopular(List()))

}

case class EnrichedMostPopular(programmes: Seq[Programme])

object EnrichedMostPopular {

  implicit object EnricherForMostPopular extends Enricher[EnrichedMostPopular, MostPopular, Programme] {
    override def apply(p: MostPopular)(children: Seq[Programme]): EnrichedMostPopular =
      EnrichedMostPopular(children)
  }

  def apply(p: MostPopular, children: Seq[Programme]): EnrichedMostPopular = EnrichedMostPopular(children)

  implicit object CachableResultForEnrichedMostPopular extends CachableResultUsingSucesses[EnrichedMostPopular]

}