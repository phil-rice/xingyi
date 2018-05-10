package one.xingyi.sample

import one.xingyi.sample.domain._
import one.xingyi.tagless.{ProfileEachEndpointLanguage, TaglessInterpreterForToString, TaglessLanguage}
import one.xingyi.core.functions.MonadCanFail
import one.xingyi.core.http.{Failer, Get, ResponseParser, ToServiceResponse}
import one.xingyi.core.json._
import one.xingyi.core.parser.Parser

import scala.language.higherKinds
import scala.concurrent.Future
import one.xingyi.core.language.Language._

class PromotionSetup[M[_], Wrapper[_, _], Fail, J: JsonParser : JsonWriter](interpreter: TaglessLanguage[M, Wrapper])(implicit
                                                                                                                      monadCanFail: MonadCanFail[M, Fail],
                                                                                                                      failer: Failer[Fail]
) extends PromotionServiceNames {

  import interpreter._
  import one.xingyi.core.endpoint.MatchesServiceRequest._

  val vogue = http(mostPopularServiceName)
  val billboard = http(promotionServiceName)
  val fnord = http(programmeAndProductionServiceName)

  val rawMostPopularService = vogue |+| objectify[MostPopularQuery, MostPopular]
  val rawPromotionService = billboard |+| objectify[PromotionQuery, Promotion]
  val rawProductionService = fnord |+| objectify[ProductionId, Production]
  val rawProgrammeService = fnord |+| objectify[ProgrammeId, Programme]

  //  val x = implicitly[ToJson[one.xingyi.sample.domain.EnrichedMostPopular]]
  val w = implicitly[FromJsonLib[J, one.xingyi.sample.domain.EnrichedMostPopular]]

  //  val x = implicitly[ToJson[EnrichedMostPopular]]
  //  val y = implicitly[ToServiceResponse[EnrichedMostPopular]]

  EnrichedMostPopular.fromJsonForEMP
  val enrichedPromotion = enrich(rawPromotionService).withChild(rawProductionService).mergeInto[EnrichedPromotion]
  val enrichedMostPopular = enrich(rawMostPopularService).withChild(rawProgrammeService).mergeInto[EnrichedMostPopular]

  val homePage = (merge(enrichedPromotion) and enrichedMostPopular into[HomePageQuery, HomePage] ((hpq, ep, emp) => HomePage(emp, ep)))

  val mostPopularEndPoint = enrichedMostPopular |+| logging("homepage") |+| endpoint[MostPopularQuery, EnrichedMostPopular]("/mostpopular", fixedPath(Get))
  val homePageEndPoint = homePage |+| logging("homepage") |+| endpoint[HomePageQuery, HomePage]("/", fixedPath(Get))
  val microservice = chain(mostPopularEndPoint, homePageEndPoint)

}

//object PromotionSetup extends App with SampleJsonsForCompilation {
//  implicit val language: TaglessInterpreterForToString = new TaglessInterpreterForToString
//
//  import TaglessInterpreterForToString._
//
//
//  import one.xingyi.core.functions.AsyncForScalaFuture._
//  import ImplicitsForTest._
//
//  val setup = new PromotionSetup[Future, StringHolder, Throwable](new ProfileEachEndpointLanguage(language.forToString))
//  println(setup.microservice.invertIndent)
//}