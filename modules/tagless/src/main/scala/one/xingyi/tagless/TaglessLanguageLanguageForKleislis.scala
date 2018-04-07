package one.xingyi.tagless

import one.xingyi.utils.cache._
import one.xingyi.utils.functions.{Async, MonadCanFailWithException}
import one.xingyi.utils.http._
import one.xingyi.utils.language.MicroserviceBuilder
import one.xingyi.utils.logging._
import one.xingyi.utils.metrics.PutMetrics
import one.xingyi.utils.time.NanoTimeService

import scala.language.{higherKinds, implicitConversions}

object TaglessLanguageLanguageForKleislis {
  def apply[M[_], Fail](implicit
                        async: Async[M],
                        monad: MonadCanFailWithException[M, Fail],
                        httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                        logReqAndResult: LogRequestAndResult[Fail],
                        timeService: NanoTimeService,
                        putMetrics: PutMetrics,
                        cacheFactory: CacheFactory[M],
                        failer: Failer[Fail],
                        responseParserFailer: ResponseParserFailer[Fail],
                        detailsLoggingForSR: DetailedLogging[ServiceResponse]) = new TaglessLanguageLanguageForKleislis[M, Fail]().NonFunctionalLanguageService()
}

class TaglessLanguageLanguageForKleislis[M[_], Fail] {
  type Kleisli[Req, Res] = Req => M[Res]

  case class NonFunctionalLanguageService(implicit
                                          protected val async: Async[M],
                                          protected val monad: MonadCanFailWithException[M, Fail],
                                          protected val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                          protected val logReqAndResult: LogRequestAndResult[Fail],
                                          protected val timeService: NanoTimeService,
                                          protected val putMetrics: PutMetrics,
                                          protected val cacheFactory: CacheFactory[M],
                                          protected val failer: Failer[Fail],
                                          protected val responseParserFailer: ResponseParserFailer[Fail],
                                          protected val detailsLoggingForSR: DetailedLogging[ServiceResponse]) extends
    TaglessLanguage[M, Kleisli] with MicroserviceBuilder[M, Fail] {
  }

}
