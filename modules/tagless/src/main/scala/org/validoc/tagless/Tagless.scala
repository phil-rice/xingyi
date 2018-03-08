package org.validoc.tagless

import org.validoc.utils.aggregate._
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.http._
import org.validoc.utils.logging._
import org.validoc.utils.metrics.ReportData
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}

import scala.language.higherKinds
import scala.reflect.ClassTag


trait TaglessLanguage[Wrapper[_, _], M[_], Fail] extends MergeLanguage[Wrapper] with EnrichLanguage[Wrapper] {


  def http(name: ServiceName): Wrapper[ServiceRequest, ServiceResponse]
  def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res): Wrapper[Req, Res]

  def objectify[Req: ClassTag, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseParser: ResponseParser[Fail, Req, Res]): Wrapper[Req, Res]
  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def retry[Req: ClassTag, Res: ClassTag:NeedsRetry](retryConfig: RetryConfig)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]


  def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])
                                            (implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): Wrapper[ServiceRequest, ServiceResponse]
  def chain(endpoints: Wrapper[ServiceRequest, ServiceResponse]*): Wrapper[ServiceRequest, ServiceResponse]

  implicit class ComposeWrapperPimper[RawReq, RawRes](wrapper: Wrapper[RawReq, RawRes]) {
    def |+|[Req, Res](fn: Wrapper[RawReq, RawRes] => Wrapper[Req, Res]): Wrapper[Req, Res] = fn(wrapper)
    //    def |++|[Req, Res](fn: Wrapper[RawReq, RawRes] => Wrapper[Req, Res]): Wrapper[Req, Res] = fn(wrapper)
  }

  def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]
  def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper[ReqM, ResM]
  def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]
  def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Wrapper[ReqM, ResM]
}





