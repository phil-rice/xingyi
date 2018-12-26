/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language

import one.xingyi.core.aggregate.{EnrichForTaglessLanguage, EnrichKleisli, MergeForTaglessLanguage, MergeKleisli}
import one.xingyi.core.cache.{CacheFactory, CacheKleisli}
import one.xingyi.core.endpoint.{ChainKleisli, EndPoint, EndpointKleisli}
import one.xingyi.core.http._
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult, LoggingKleisli}
import one.xingyi.core.metrics.{MetricsKleisli, PutMetrics}
import one.xingyi.core.monad.{Async, LiftFunctionKleisli, Monad, MonadCanFailWithException}
import one.xingyi.core.objectify.{ObjectifyKleisli, XingyiKleisli}
import one.xingyi.core.profiling.ProfileKleisli
import one.xingyi.core.retry.RetryKleisli
import one.xingyi.core.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag

trait MicroserviceComposers[M[_]] {

  implicit class ComposeWrapperPimper[RawReq, RawRes](wrapper: RawReq => M[RawRes]) {
    def |+|[Req, Res](fn: (RawReq => M[RawRes]) => (Req => M[Res])): (Req => M[Res]) = fn(wrapper)
    def |++|[Req, Res](fn: (RawReq => M[RawRes]) => EndPoint[M, Req, Res]): EndPoint[M, Req, Res] = fn(wrapper)
  }

}



trait AndAfterKleisli[M[_]] {
  protected implicit def monad: Monad[M]

  def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Req => M[Mid], fn: Mid => Res2): Req => M[Res2] = {
    req: Req => monad.map(raw(req), fn)
  }
  def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Req => M[Mid], fn: Mid => M[Res2]): Req => M[Res2] = {
    req: Req => monad.flatMap(raw(req), fn)
  }

}

trait MicroserviceBuilder[M[_], Fail] extends ObjectifyKleisli[M, Fail] with  XingyiKleisli[M, Fail] with HttpKlesili[M] with MetricsKleisli[M, Fail] with LoggingKleisli[M, Fail]
  with ChainKleisli[M, Fail] with EndpointKleisli[M] with RetryKleisli[M, Fail] with ProfileKleisli[M, Fail] with CacheKleisli[M] with LiftFunctionKleisli[M]
  with MergeKleisli[M] with EnrichKleisli[M] with AndAfterKleisli[M] {

  protected implicit def async: Async[M]
  protected implicit def monad: MonadCanFailWithException[M, Fail]
  protected def cacheFactory: CacheFactory[M]
  protected implicit def timeService: NanoTimeService
  protected def logReqAndResult: LogRequestAndResult[Fail]
  protected def failer: Failer[Fail]
  protected def putMetrics: PutMetrics

  def debugEndpoints(endpoints: Map[String, String])(original: ServiceRequest => M[Option[ServiceResponse]]) = original


}

//class KleisliMicroserviceBuilder[M[_], Fail] {
//  type Klesli[Req, Res] = Req => M[Res]
//
//  case class builder()(implicit protected val async: Async[M],
//                       protected val monad: MonadCanFailWithException[M, Fail],
//                       protected val timeService: NanoTimeService,
//                       protected val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
//                       protected val cacheFactory: CacheFactory[M],
//                       protected val logReqAndResult: LogRequestAndResult[Fail],
//                       protected val failer: Failer[Fail],
//                       protected val putMetrics: PutMetrics,
//                       protected val responseParserFailer: ResponseParserFailer[Fail],
//                       protected val detailsLoggingForSR: DetailedLogging[ServiceResponse]
//
//  ) extends MicroserviceBuilder[M, Fail] with MergeLanguage[Klesli] with EnrichLanguage[Klesli] with MicroserviceComposers[M]
//
//}
