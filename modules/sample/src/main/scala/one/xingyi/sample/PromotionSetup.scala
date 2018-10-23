/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample
import one.xingyi.core.aggregate.{EnrichLanguage, MergeLanguage}
import one.xingyi.core.cache.CacheFactory
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.{MicroserviceBuilder, MicroserviceComposers}
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult}
import one.xingyi.core.metrics.PutMetrics
import one.xingyi.core.monad.{Async, Liftable, MonadCanFail, MonadCanFailWithException}
import one.xingyi.core.time.NanoTimeService
import one.xingyi.sample.domain._

import scala.language.higherKinds

class PromotionSetup[M[_],  Fail, J: JsonParser : JsonWriter](implicit val monad: MonadCanFailWithException[M, Fail],
                                                                            val async: Async[M],
                                                                            val failer: Failer[Fail],
                                                                            val cacheFactory: CacheFactory[M],
                                                                            val timeService: NanoTimeService,
                                                                            val logReqAndResult: LogRequestAndResult[Fail],
                                                                            val putMetrics: PutMetrics,
                                                                            val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                                                            val detailedLoggingForSR: DetailedLogging[ServiceResponse])
  extends PromotionServiceNames with MicroserviceBuilder[M, Fail] with MicroserviceComposers[M] with EnrichLanguage[M] with MergeLanguage[M] {

  import one.xingyi.core.endpoint.MatchesServiceRequest._

  val vogue = http(mostPopularServiceName)
  val billboard = http(promotionServiceName)
  val fnord = http(programmeAndProductionServiceName)

  val rawMostPopularService = vogue |+| objectify[MostPopularQuery, MostPopular]
  val rawPromotionService = billboard |+| objectify[PromotionQuery, Promotion]
  val rawProductionService = fnord |+| objectify[ProductionId, Production]
  val rawProgrammeService = fnord |+| objectify[ProgrammeId, Programme]

  val enrichedPromotion: PromotionQuery => M[EnrichedPromotion] = enrich(rawPromotionService).withChild(rawProductionService).mergeInto[EnrichedPromotion]
  val enrichedMostPopular: MostPopularQuery => M[EnrichedMostPopular] = enrich(rawMostPopularService).withChild(rawProgrammeService).mergeInto[EnrichedMostPopular]

  val homePage: HomePageQuery => M[HomePage] = (merge(enrichedPromotion) and enrichedMostPopular into[HomePageQuery, HomePage] ((hpq, ep, emp) => HomePage(emp, ep)))

  val x = MostPopularQuery.fromServiceRequest[M]

  val mostPopularEndPoint = enrichedMostPopular |+| logging[MostPopularQuery, EnrichedMostPopular]("homepage") |+| endpoint[MostPopularQuery, EnrichedMostPopular]("/mostpopular", fixedPath(Get))
  val homePageEndPoint = homePage |+| logging[HomePageQuery, HomePage]("homepage") |+| endpoint[HomePageQuery, HomePage]("/", fixedPath(Get))
  val microservice = chain(mostPopularEndPoint, homePageEndPoint)
}

