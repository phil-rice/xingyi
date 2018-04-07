package one.xingyi.sample

import one.xingyi.sample.domain._
import one.xingyi.tagless.{ProfileEachEndpointLanguage, TaglessInterpreterForToString, TaglessLanguage}
import one.xingyi.utils.functions.MonadCanFail
import one.xingyi.utils.http.{Failer, Get}
import scala.language.higherKinds
import scala.concurrent.Future

class PromotionSetup[ M[_],Wrapper[_, _], Fail](interpreter: TaglessLanguage[M, Wrapper])(implicit
                                                                                                monadCanFail: MonadCanFail[M, Fail],
                                                                                                failer: Failer[Fail],
                                                                                                jsonBundle: JsonBundle
) extends PromotionServiceNames {

  import interpreter._
  import jsonBundle._
  import one.xingyi.utils.endpoint.MatchesServiceRequest._

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

object PromotionSetup extends App with SampleJsonsForCompilation {
  implicit val language: TaglessInterpreterForToString = new TaglessInterpreterForToString

  import TaglessInterpreterForToString._

  implicit val jsonBundle: JsonBundle = JsonBundle()

  import one.xingyi.utils.functions.AsyncForScalaFuture._
  import ImplicitsForTest._

  val setup = new PromotionSetup[Future, StringHolder,  Throwable](new ProfileEachEndpointLanguage(language.forToString))
  println(setup.microservice.invertIndent)
}