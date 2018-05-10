package one.xingyi.sample.domain

import one.xingyi.core.aggregate.FindReq
import one.xingyi.core.cache.{CachableKey, UnitId}
import one.xingyi.core.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.core.functions.Liftable
import one.xingyi.core.http._
import one.xingyi.core.json._
//import io.circe.syntax._
import scala.language.higherKinds
import one.xingyi.core.language.Language._

// need this, but it may be removed by 'organise imports' import io.circe.generic.auto._
import one.xingyi.core.language.Language._

import scala.language.implicitConversions

case class HomePage(mostPopular: EnrichedMostPopular, promotions: EnrichedPromotion)

object HomePage extends DomainResponseCompanionObject[HomePageQuery, HomePage] {
//  implicit def homePageToJson[J: JsonWriter](implicit forMostPopular: ToJsonLib[EnrichedMostPopular], forPromotions: ToJsonLib[EnrichedPromotion]): ToJsonLib[HomePage] = {
//    homePage => JsonObject("mostPopular" -> forMostPopular(homePage.mostPopular), "promotions" -> forPromotions(homePage.promotions))
//  }
}

case class HomePageQuery(bypassCache: Boolean) extends BypassCache

object HomePageQuery extends DomainRequestCompanionQuery[HomePageQuery] {

  implicit def fromServiceRequestForHomePageQuery[M[_] : Liftable] = new FromServiceRequestForHomePageQuery[M]

  class FromServiceRequestForHomePageQuery[M[_] : Liftable] extends FromServiceRequest[M, HomePageQuery] {
    override def apply(v1: ServiceRequest) = HomePageQuery(false).liftM
  }
  //
  //  implicit def fromJsonForHomePageQuery[J: JsonParser] = new FromJsonLib[J, HomePageQuery] {
  //    override def apply(json: J): HomePageQuery = HomePageQuery(false)
  //  }

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



