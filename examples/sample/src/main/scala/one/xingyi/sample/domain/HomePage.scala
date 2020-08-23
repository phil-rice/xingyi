/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain

import one.xingyi.core.aggregate.FindReq
import one.xingyi.core.cache.{CachableKey, UnitId}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.Liftable
//import io.circe.syntax._
import scala.language.higherKinds

// need this, but it may be removed by 'organise imports' import io.circe.generic.auto._
import one.xingyi.core.language.Language._

import scala.language.implicitConversions

case class HomePage(mostPopular: EnrichedMostPopular, promotions: EnrichedPromotion)

object HomePage  {
  implicit def homePageToJson[J: JsonWriter](implicit forMostPopular: ToJsonLib[EnrichedMostPopular], forPromotions: ToJsonLib[EnrichedPromotion]): ToJsonLib[HomePage] = {
    homePage => JsonObject("mostPopular" -> forMostPopular(homePage.mostPopular), "promotions" -> forPromotions(homePage.promotions))
  }
}

case class HomePageQuery(bypassCache: Boolean)

object HomePageQuery  {

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



