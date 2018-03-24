package org.validoc.tagless

import org.validoc.utils.aggregate._
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.http._
import org.validoc.utils.logging._
import org.validoc.utils.metrics.ReportData
import org.validoc.utils.profiling.{ProfileAs, TryProfileData}
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}

import scala.language.higherKinds
import scala.reflect.ClassTag


trait TaglessLanguage[Wrapper[_, _], M[_]] extends MergeLanguage[Wrapper] with EnrichLanguage[Wrapper] {
  def as[Wrapper2[_, _]](implicit ev: Wrapper[_, _] <:< Wrapper2[_, _]): TaglessLanguage[Wrapper2, M] = this.asInstanceOf[TaglessLanguage[Wrapper2, M]]


  def http(name: ServiceName): Wrapper[ServiceRequest, ServiceResponse]
  def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res): Wrapper[Req, Res]

  def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): Wrapper[Req, Res]
  def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => Res2): Wrapper[Req, Res2]
  def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => M[Res2]): Wrapper[Req, Res2]
  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]


  def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])
                                            (implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): Wrapper[ServiceRequest, Option[ServiceResponse]]
  def chain(endpoints: Wrapper[ServiceRequest, Option[ServiceResponse]]*): Wrapper[ServiceRequest, Option[ServiceResponse]]
  def debugEndpoints(endpoints: Map[String, String])(original: Wrapper[ServiceRequest, Option[ServiceResponse]]): Wrapper[ServiceRequest, Option[ServiceResponse]]

  implicit class ComposeWrapperPimper[RawReq, RawRes](wrapper: Wrapper[RawReq, RawRes]) {
    def |+|[Req, Res](fn: Wrapper[RawReq, RawRes] => Wrapper[Req, Res]): Wrapper[Req, Res] = fn(wrapper)
  }

  def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]
  def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper[ReqM, ResM]
  def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]
  def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Wrapper[ReqM, ResM]
}


class TwinTaglessLanguage[Wrapper1[_, _], Wrapper2[_, _], M[_]](lang1: TaglessLanguage[Wrapper1, M], lang2: TaglessLanguage[Wrapper2, M]) {
  case class TwinTagless[Req, Res](wrapper1: Wrapper1[Req, Res], wrapper2: Wrapper2[Req, Res])

  class language extends TaglessLanguage[TwinTagless, M] {
    override def http(name: ServiceName) = TwinTagless(lang1.http(name), lang2.http(name))
    override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) = TwinTagless(lang1.function(name)(fn), lang2.function(name)(fn))
    override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: TwinTagless[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]) =
      TwinTagless(lang1.objectify[Req, Res](http.wrapper1), lang2.objectify[Req, Res](http.wrapper2))
    override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: TwinTagless[Req, Mid], fn: Mid => Res2) =
      TwinTagless(lang1.andAfter[Req, Mid, Res2](raw.wrapper1, fn), lang2.andAfter[Req, Mid, Res2](raw.wrapper2, fn))
    override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: TwinTagless[Req, Mid], fn: Mid => M[Res2]) =
      TwinTagless(lang1.andAfterK[Req, Mid, Res2](raw.wrapper1, fn), lang2.andAfterK[Req, Mid, Res2](raw.wrapper2, fn))
    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: TwinTagless[Req, Res]) =
      TwinTagless(lang1.logging[Req, Res](messagePrefix)(raw.wrapper1), lang2.logging[Req, Res](messagePrefix)(raw.wrapper2))
    override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: TwinTagless[Req, Res]) =
      TwinTagless(lang1.metrics[Req, Res](prefix)(raw.wrapper1), lang2.metrics[Req, Res](prefix)(raw.wrapper2))
    override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: TwinTagless[Req, Res]) =
      TwinTagless(lang1.cache[Req, Res](name)(raw.wrapper1), lang2.cache[Req, Res](name)(raw.wrapper2))
    override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: TwinTagless[Req, Res]) =
      TwinTagless(lang1.retry[Req, Res](retryConfig)(raw.wrapper1), lang2.retry[Req, Res](retryConfig)(raw.wrapper2))
    override def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: TwinTagless[Req, Res]) =
      TwinTagless(lang1.profile[Req, Res](profileData)(raw.wrapper1), lang2.profile[Req, Res](profileData)(raw.wrapper2))

    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: TwinTagless[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) =
      TwinTagless(lang1.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw.wrapper1), lang2.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw.wrapper2))

    override def chain(endpoints: TwinTagless[ServiceRequest, Option[ServiceResponse]]*) =
      TwinTagless(lang1.chain(endpoints.map(_.wrapper1): _*), lang2.chain(endpoints.map(_.wrapper2): _*))
    override def debugEndpoints(endpoints: Map[String, String])(original: TwinTagless[ServiceRequest, Option[ServiceResponse]]) = original
    override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: TwinTagless[ReqP, ResP], child: TwinTagless[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      TwinTagless(lang1.enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent.wrapper1, child.wrapper1), lang2.enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent.wrapper2, child.wrapper2))
    override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: TwinTagless[Req1, Res1], secondService: TwinTagless[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) =
      TwinTagless(lang1.merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService.wrapper1, secondService.wrapper1, merger), lang2.merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService.wrapper2, secondService.wrapper2, merger))
    override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: TwinTagless[Req1, Res1], secondService: TwinTagless[Req2, Res2], thirdService: TwinTagless[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) =
      TwinTagless(lang1.merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService.wrapper1, secondService.wrapper1, thirdService.wrapper1, merger), lang2.merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService.wrapper2, secondService.wrapper2, thirdService.wrapper2, merger))
    override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: TwinTagless[Req1, Res1], secondService: TwinTagless[Req2, Res2], thirdService: TwinTagless[Req3, Res3], fourthService: TwinTagless[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4) =
      TwinTagless(lang1.merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService.wrapper1, secondService.wrapper1, thirdService.wrapper1, fourthService.wrapper1, merger), lang2.merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService.wrapper2, secondService.wrapper2, thirdService.wrapper2, fourthService.wrapper2, merger))
  }

}

