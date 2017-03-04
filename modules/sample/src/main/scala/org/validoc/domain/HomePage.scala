package org.validoc.domain

import org.validoc.utils.{FromServiceRequest, ToServiceRequest, ToServiceResponse}
import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, UnitId}
import org.validoc.utils.http._

case class HomePage(mostPopular: EnrichedMostPopular, promotions: EnrichedPromotion)


object HomePage {

  implicit object CachableResultForHomePage extends CachableResultUsingSucesses[HomePage]

  implicit def toServiceResponseForHomePage(v1: HomePage) = ServiceResponse(Status.Ok, Body(v1.toString), ContentType("text/plain"))

}

trait HomePageQuery

object HomePageQuery extends HomePageQuery {


  implicit object CachableKeyForHomePage extends CachableKey[HomePageQuery] {
    override def id(req: HomePageQuery): Id = UnitId

    override def bypassCache(req: HomePageQuery): Boolean = false
  }

  implicit def toRequestForHomePageQuery(req: HomePageQuery) = ServiceRequest(Get, Uri("someUri"))

  implicit def fromServiceRequestForHomePageQuery(v1: ServiceRequest): HomePageQuery = HomePageQuery

}



