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

class PromotionSetup[Wrapper[_, _], Fail, HttpReq: ClassTag:Cachable:ShouldCache, HttpRes: ClassTag]
(implicit
 interpreter: TaglessLanguage[Wrapper, Fail, HttpReq, HttpRes],
 failer: Failer[Fail],
 toJsonForHomePage: ToJson[HomePage],
 toJsonForEnrichedMostPopular: ToJson[EnrichedMostPopular],
 fromJsonForMostPopular: FromJson[MostPopular],
 fromJsonForPromotion: FromJson[Promotion],
 fromJsonForProgramme: FromJson[Programme],
 fromJsonForProduction: FromJson[Production]
) extends PromotionServiceNames {

  import interpreter._


  val vogue = http(mostPopularServiceName)

  val billboard = http(promotionServiceName)

  val fnord = http(programmeAndProductionServiceName)


  val x = Promotion.responseParser[Fail]
  ResponseProcessor.defaultResponseProcessor[Fail, MostPopularQuery, MostPopular]
  ResponseProcessor.defaultResponseProcessor[Fail, PromotionQuery, Promotion]
  ResponseProcessor.defaultResponseProcessor[Fail, ProductionId, Production]
  val rawMostPopularService = vogue |+| objectify[MostPopularQuery, MostPopular] |+| cache("vogue")
  val rawPromotionService = billboard |+| cache("Promotion") |+| objectify[PromotionQuery, Promotion]
  val rawProductionService = fnord |+| objectify[ProductionId, Production]
  val rawProgrammeService = fnord |+| objectify[ProgrammeId, Programme]


  val enrichedPromotion = enrich(rawPromotionService).withChild(rawProductionService).mergeInto[EnrichedPromotion]
  val enrichedMostPopular = enrich(rawMostPopularService).withChild(rawProgrammeService).mergeInto[EnrichedMostPopular]

  val homePage = merge(enrichedPromotion) and enrichedMostPopular into[HomePageQuery, HomePage] ((hpq, ep, emp) => HomePage(emp, ep))


}

