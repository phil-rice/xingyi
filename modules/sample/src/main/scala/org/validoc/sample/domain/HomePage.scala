package org.validoc.sample.domain

import org.validoc.utils.gash.{FindId, Merger}
//import io.circe.syntax._

// need this, but it may be removed by 'organise imports' import io.circe.generic.auto._
import org.validoc.utils.caching.{CachableKey, Id, UnitId}
import org.validoc.utils.http._

import scala.language.implicitConversions

case class HomePage(mostPopular: EnrichedMostPopular, promotions: EnrichedPromotion)

object HomePage extends DomainCompanionObject[HomePage] {

  implicit object MergerForHomePage extends Merger[EnrichedPromotion, EnrichedMostPopular, HomePage] {
    override def apply(v1: EnrichedPromotion, v2: EnrichedMostPopular): HomePage = HomePage(v2, v1)
  }

}


trait HomePageQuery

object HomePageQuery extends DomainCompanionObject[HomePageQuery] with HomePageQuery {

  implicit object FindPromotionQuery extends FindId[HomePageQuery, PromotionQuery] {
    override def apply(v1: HomePageQuery): PromotionQuery = PromotionQuery
  }

  implicit object FindMostPopularQuery extends FindId[HomePageQuery, MostPopularQuery] {
    override def apply(v1: HomePageQuery): MostPopularQuery = MostPopularQuery
  }

  implicit object CachableKeyForHomePage extends CachableKey[HomePageQuery] {
    override def id(req: HomePageQuery): Id = UnitId

    override def bypassCache(req: HomePageQuery): Boolean = false
  }

  implicit def toRequestForHomePageQuery(req: HomePageQuery) = ServiceRequest(Get, Uri("someUri"))

  implicit object FromServiceRequestForHomPageQuery extends FromServiceRequest[HomePageQuery] {
    override def apply(v1: ServiceRequest): HomePageQuery = HomePageQuery
  }

}



