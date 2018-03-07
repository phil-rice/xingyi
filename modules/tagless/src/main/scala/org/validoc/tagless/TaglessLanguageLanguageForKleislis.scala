package org.validoc.tagless

import org.validoc.utils.cache._
import org.validoc.utils.endpoint.EndPoint
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.http._
import org.validoc.utils.language.MicroserviceBuilder
import org.validoc.utils.logging._
import org.validoc.utils.metrics.PutMetrics
import org.validoc.utils.time.NanoTimeService

import scala.language.{higherKinds, implicitConversions}


class TaglessLanguageLanguageForKleislis[M[_], Fail] {
  type Kleisli[Req, Res] = Req => M[Res]
  type EndpointK[Req, Res] = EndPoint[M, Req, Res]

  case class NonFunctionalLanguageService(implicit
                                          protected val async: Async[M],
                                          protected val monad: MonadCanFailWithException[M, Fail],
                                          protected val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                          protected val logReqAndResult: LogRequestAndResult[Fail],
                                          protected val timeService: NanoTimeService,
                                          protected val putMetrics: PutMetrics,
                                          protected val cacheFactory: CacheFactory[M],
                                          protected val failer: Failer[M, Fail]) extends
    TaglessLanguage[EndpointK, Kleisli, M, Fail] with MicroserviceBuilder[M, Fail]
}
