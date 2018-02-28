package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.cache.{Cachable, ShouldCache}
import org.validoc.utils.http._
import org.validoc.utils.json.{FromJson, ToJson}
import org.validoc.utils.tagless.TaglessLanguage

import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag


trait PromotionServiceNames {
  val mostPopularServiceName = ServiceName("mostPopular")
  val promotionServiceName = ServiceName("promotion")
  val programmeAndProductionServiceName = ServiceName("programmeAndProduction")
}

case class JsonBundle(implicit
                      val toJsonForHomePage: ToJson[HomePage],
                      val toJsonForEnrichedMostPopular: ToJson[EnrichedMostPopular],
                      val fromJsonForMostPopular: FromJson[MostPopular],
                      val fromJsonForPromotion: FromJson[Promotion],
                      val fromJsonForProgramme: FromJson[Programme],
                      val fromJsonForProduction: FromJson[Production])

class PromotionSetup[EndpointWrapper[_,_], Wrapper[_, _], Fail, HttpReq: ClassTag : Cachable : ShouldCache, HttpRes: ClassTag]
(implicit
 interpreter: TaglessLanguage[EndpointWrapper, Wrapper, Fail, HttpReq, HttpRes],
 failer: Failer[Fail],
 jsonBundle: JsonBundle
) extends PromotionServiceNames {

  import interpreter._
  import jsonBundle._


  val vogue = http(mostPopularServiceName)
  val billboard = http(promotionServiceName)
  val fnord = http(programmeAndProductionServiceName)

  val rawMostPopularService = vogue |+| objectify[MostPopularQuery, MostPopular] |+| cache("vogue")
  val rawPromotionService = billboard |+| cache("Promotion") |+| objectify[PromotionQuery, Promotion]
  val rawProductionService = fnord |+| objectify[ProductionId, Production]
  val rawProgrammeService = fnord |+| objectify[ProgrammeId, Programme]


  val enrichedPromotion = enrich(rawPromotionService).withChild(rawProductionService).mergeInto[EnrichedPromotion]
  val enrichedMostPopular = enrich(rawMostPopularService).withChild(rawProgrammeService).mergeInto[EnrichedMostPopular]

  val homePage = merge(enrichedPromotion) and enrichedMostPopular into[HomePageQuery, HomePage] ((hpq, ep, emp) => HomePage(emp, ep))


}

