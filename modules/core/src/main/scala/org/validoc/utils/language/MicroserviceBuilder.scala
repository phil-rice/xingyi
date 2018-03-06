package org.validoc.utils.language

import org.validoc.utils.aggregate.{EnrichKleisli, MergeKleisli}
import org.validoc.utils.cache.CacheKleisli
import org.validoc.utils.endpoint.{ChainKleisli, EndPoint, EndpointKleisli}
import org.validoc.utils.functions.{Async, LiftFunctionKleisli, MonadCanFailWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging.{LogRequestAndResult, LoggingKleisli}
import org.validoc.utils.metrics.{MetricsKleisli, PutMetrics}
import org.validoc.utils.objectify.ObjectifyKleisli
import org.validoc.utils.profiling.ProfileKleisli
import org.validoc.utils.retry.RetryKleisli
import org.validoc.utils.time.NanoTimeService

trait MicroserviceComposers[M[_]] {
  implicit class ComposeWrapperPimper[RawReq, RawRes](wrapper: RawReq => M[RawRes]) {
    def |+|[Req, Res](fn: (RawReq => M[RawRes]) => (Req => M[Res])): (Req => M[Res]) = fn(wrapper)
  }
  implicit class ComposeWrapper2Pimper[RawReq, RawRes](wrapper: RawReq => M[RawRes]) {
    def |++|[Req, Res](fn: (RawReq => M[RawRes]) => EndPoint[M, Req, Res]): EndPoint[M, Req, Res] = fn(wrapper)
  }
}

trait MicroserviceBuilder[M[_], Fail] extends ObjectifyKleisli[M, Fail] with HttpKlesili[M] with MetricsKleisli[M, Fail] with LoggingKleisli[M, Fail]
  with ChainKleisli[M, Fail] with EndpointKleisli[M] with RetryKleisli[M, Fail] with ProfileKleisli[M, Fail] with CacheKleisli[M] with LiftFunctionKleisli[M]
  with MergeKleisli[M] with EnrichKleisli[M] {


  protected implicit def async: Async[M]
  protected implicit def monad: MonadCanFailWithException[M, Fail]

  protected implicit def timeService: NanoTimeService
  protected def logReqAndResult: LogRequestAndResult[Fail]
  protected def failer: Failer[Fail]
  protected def putMetrics: PutMetrics

}
