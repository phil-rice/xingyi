package org.validoc.sample.domain

import org.validoc.utils.aggregate.FindReq
import org.validoc.utils.cache.Cachable
//import io.circe.syntax._

// need this, but it may be removed by 'organise imports' import io.circe.generic.auto._
import org.validoc.utils.http._

import scala.language.implicitConversions

case class HomePage(mostPopular: EnrichedMostPopular, promotions: EnrichedPromotion)

object HomePage extends DomainCompanionObject[HomePageQuery, HomePage]

case class HomePageQuery(bypassCache: Boolean) extends BypassCache

object HomePageQuery extends DomainCompanionQuery[HomePageQuery] {

  implicit object FindPromotionQuery extends FindReq[HomePageQuery, PromotionQuery] {
    override def apply(v1: HomePageQuery): PromotionQuery = PromotionQuery(v1.bypassCache)
  }

  implicit object FindMostPopularQuery extends FindReq[HomePageQuery, MostPopularQuery] {
    override def apply(v1: HomePageQuery): MostPopularQuery = MostPopularQuery(v1.bypassCache)
  }

  implicit object CachableKeyForHomePage extends Cachable[HomePageQuery] {
    override def apply(v1: HomePageQuery) = ()
  }

}



