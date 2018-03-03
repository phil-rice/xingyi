package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.cache.{Cachable, ShouldCache}
import org.validoc.utils.functions.{Liftable, MonadCanFail}
import org.validoc.utils.http._
import org.validoc.utils.json.{FromJson, ToJson}
import org.validoc.utils.tagless.{TaglessInterpreterForToString, TaglessLanguage}

import scala.concurrent.Future
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

class PromotionSetup[EndpointWrapper[_, _], Wrapper[_, _], M[_], Fail, HttpReq: ClassTag : Cachable : ShouldCache, HttpRes: ClassTag](interpreter: TaglessLanguage[EndpointWrapper, Wrapper, M, Fail])(implicit
                                                                                                                                                                                                       monadCanFail: MonadCanFail[M, Fail],
                                                                                                                                                                                                       failer: Failer[Fail],
                                                                                                                                                                                                       jsonBundle: JsonBundle
) extends PromotionServiceNames {

  import interpreter._
  import jsonBundle._

  import org.validoc.utils.endpoint.MatchesServiceRequest._

  val vogue = http(mostPopularServiceName)
  val billboard = http(promotionServiceName)
  val fnord = http(programmeAndProductionServiceName)

  val rawMostPopularService = vogue |+| objectify[MostPopularQuery, MostPopular]
  // |+| cache("vogue")
  val rawPromotionService = billboard |+| cache("Promotion") |+| objectify[PromotionQuery, Promotion]
  val rawProductionService = fnord |+| objectify[ProductionId, Production]
  val rawProgrammeService = fnord |+| objectify[ProgrammeId, Programme]


  val enrichedPromotion = enrich(rawPromotionService).withChild(rawProductionService).mergeInto[EnrichedPromotion]
  val enrichedMostPopular = enrich(rawMostPopularService).withChild(rawProgrammeService).mergeInto[EnrichedMostPopular]

  val homePage1 = merge(enrichedPromotion).and(enrichedMostPopular).into[HomePageQuery, HomePage]((hpq, ep, emp) => HomePage(emp, ep))
  val homePage: Wrapper[HomePageQuery, HomePage] = merge(enrichedPromotion) and enrichedMostPopular into[HomePageQuery, HomePage] ((hpq, ep, emp) => HomePage(emp, ep))


  val mostPopularEndPoint = enrichedMostPopular |++| endpoint[MostPopularQuery, EnrichedMostPopular]("/", fixedPath(Get))

  val homePageEndPoint = homePage |++| endpoint[HomePageQuery, HomePage]("/", fixedPath(Get))
  val microservice = chain(mostPopularEndPoint, homePageEndPoint)


}

object PromotionSetup extends App with SampleJsonsForCompilation {
  implicit val language: TaglessInterpreterForToString = new TaglessInterpreterForToString

  import language._

  implicit val jsonBundle: JsonBundle = JsonBundle()

  import org.validoc.utils.functions.AsyncForScalaFuture._
  import ImplicitsForTest._

  val setup = new PromotionSetup[StringHolder, StringHolder, Future, Throwable, String, String](language.forToString)
  println(setup.microservice.invertIndent)
}
