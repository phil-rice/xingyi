package org.validoc.domain

import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, UnitId}
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}

case class HomePage(mostPopular: EnrichedMostPopular, promotions: EnrichedPromotion)


object HomePage{

  implicit object CachableResultForHomePage extends CachableResultUsingSucesses[HomePage]

}

trait HomePageQuery

object HomePageQuery extends HomePageQuery {

  implicit object CachableKeyForHomePage extends CachableKey[HomePageQuery] {
    override def id(req: HomePageQuery): Id = UnitId

    override def bypassCache(req: HomePageQuery): Boolean = false
  }

  implicit object ToRequestForHomePageQuery extends ToServiceRequest[HomePageQuery] {
    override def apply(req: HomePageQuery): ServiceRequest =
      ServiceRequest(Get, Uri("someUri"))
  }
}



