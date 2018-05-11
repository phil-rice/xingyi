package one.xingyi.sample

import one.xingyi.core.functions.MonadCanFail
import one.xingyi.core.http.{Failer, Get}
import one.xingyi.core.json._
import one.xingyi.sample.domain._
import one.xingyi.tagless.TaglessLanguage

import scala.language.higherKinds

class PromotionSetup[M[_], Wrapper[_, _], Fail, J: JsonParser : JsonWriter](interpreter: TaglessLanguage[M, Wrapper])(implicit monadCanFail: MonadCanFail[M, Fail], failer: Failer[Fail]) extends PromotionServiceNames {

  import interpreter._
  import one.xingyi.core.endpoint.MatchesServiceRequest._

  val vogue = http(mostPopularServiceName)
  val billboard = http(promotionServiceName)
  val fnord = http(programmeAndProductionServiceName)

  val rawMostPopularService = vogue |+| objectify[MostPopularQuery, MostPopular]
  val rawPromotionService = billboard |+| objectify[PromotionQuery, Promotion]
  val rawProductionService = fnord |+| objectify[ProductionId, Production]
  val rawProgrammeService = fnord |+| objectify[ProgrammeId, Programme]

  val enrichedPromotion = enrich(rawPromotionService).withChild(rawProductionService).mergeInto[EnrichedPromotion]
  val enrichedMostPopular = enrich(rawMostPopularService).withChild(rawProgrammeService).mergeInto[EnrichedMostPopular]

  val homePage = (merge(enrichedPromotion) and enrichedMostPopular into[HomePageQuery, HomePage] ((hpq, ep, emp) => HomePage(emp, ep)))

  val mostPopularEndPoint = enrichedMostPopular |+| logging("homepage") |+| endpoint[MostPopularQuery, EnrichedMostPopular]("/mostpopular", fixedPath(Get))
  val homePageEndPoint = homePage |+| logging("homepage") |+| endpoint[HomePageQuery, HomePage]("/", fixedPath(Get))
  val microservice = chain(mostPopularEndPoint, homePageEndPoint)
}

