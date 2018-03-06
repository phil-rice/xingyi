package org.validoc.sample.domain

import org.validoc.utils.aggregate.FindReq
import org.validoc.utils.cache.{CachableKey, UnitId}
import org.validoc.utils.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import org.validoc.utils.functions.Liftable
import org.validoc.utils.http._
//import io.circe.syntax._
import scala.language.higherKinds


// need this, but it may be removed by 'organise imports' import io.circe.generic.auto._
import org.validoc.utils.language.Language._

import scala.language.implicitConversions

case class HomePage(mostPopular: EnrichedMostPopular, promotions: EnrichedPromotion)

object HomePage extends DomainResponseCompanionObject[HomePageQuery, HomePage]

case class HomePageQuery(bypassCache: Boolean) extends BypassCache

object HomePageQuery extends DomainRequestCompanionQuery[HomePageQuery] {

  implicit def fromServiceRequestForHomePageQuery[M[_] : Liftable] = new FromServiceRequestForHomePageQuery[M]

  class FromServiceRequestForHomePageQuery[M[_] : Liftable] extends FromServiceRequest[M, HomePageQuery] {
    override def apply(v1: ServiceRequest) = HomePageQuery(false).liftM
  }

  implicit object FindPromotionQuery extends FindReq[HomePageQuery, PromotionQuery] {
    override def apply(v1: HomePageQuery): PromotionQuery = PromotionQuery(v1.bypassCache)
  }

  implicit object FindMostPopularQuery extends FindReq[HomePageQuery, MostPopularQuery] {
    override def apply(v1: HomePageQuery): MostPopularQuery = MostPopularQuery(v1.bypassCache)
  }

  implicit object CachableKeyForHomePage extends CachableKey[HomePageQuery] {
//    override def apply(v1: HomePageQuery) = ()
    override def id(req: HomePageQuery) = UnitId
    override def bypassCache(req: HomePageQuery) = req.bypassCache
  }

}



