package org.validoc.utils.tagless

import org.validoc.utils._
import org.validoc.utils.cache._
import org.validoc.utils.concurrency.Async
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.logging._
import org.validoc.utils.metrics.PutMetrics
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.{NeedsRetry, RetryConfig, RetryService}
import org.validoc.utils.success.MessageName
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag

trait HttpFactory[M[_], HttpReq, HttpRes] extends (ServiceName => HttpReq => M[HttpRes])

class TaglessLanguageLanguageForKleislis[M[_] : Async, Fail, HttpReq, HttpRes](implicit monadCanFail: MonadCanFail[M, Fail],
                                                                               httpFactory: HttpFactory[M, HttpReq, HttpRes],
                                                                               toServiceResponse: ToServiceResponse[HttpRes],
                                                                               toHttpReq: FromServiceRequest[HttpReq],
                                                                               logReqAndResult: LogRequestAndResult[Fail],
                                                                               loggingAdapter: LoggingAdapter,
                                                                               timeService: NanoTimeService,
                                                                               putMetrics: PutMetrics,
                                                                               cacheFactory: CacheFactory[M]) {
  type K[Req, Res] = Req => M[Res]
  type KOpt[Req, Res] = Req => Option[M[Res]]


  class NonFunctionalLanguageService extends TaglessLanguage[KOpt, K, Fail, HttpReq, HttpRes] {
    override def http(name: ServiceName) = httpFactory(name)

    override def metrics[Req: ClassTag, Res: ClassTag](prefix: String)(raw: K[Req, Res])(implicit makeReportData: RD[Res]) =
      raw.enterAndExit[Fail, Long]({ r: Req => timeService() }, makeReportData(prefix) ~> putMetrics)

    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: K[Req, Res])(implicit fromServiceRequest: FromServiceRequest[Req], toServiceResponse: ToServiceResponse[Res]): KOpt[ServiceRequest, ServiceResponse] =
      {serviceRequest => matchesServiceRequest(normalisedPath)(serviceRequest).toOption((fromServiceRequest ~> raw |=> toServiceResponse) (serviceRequest))}
//    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest) (implicit fromServiceRequest: FromServiceRequest[Req], toServiceResponse: ToServiceResponse[Res])(raw: K[Req, Res]): K[Req, Res] =

    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](pattern: String)(raw: K[Req, Res])(implicit messageName: MessageName[Req, Res]) =
      raw.sideeffect(logReqAndResult[Req, Res](raw))

    override def cache[Req: ClassTag : Cachable : ShouldCache, Res: ClassTag](name: String)(raw: K[Req, Res]): Req => M[Res] =
      Cache[M, Req, Res](cacheFactory[Req, Res](name, raw))

    override def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: K[Req, Res])(implicit retry: NeedsRetry[Fail, Res]): K[Req, Res] =
      new RetryService[M, Fail, Req, Res](raw, retryConfig)
    override def profile[Req: ClassTag, Res: ClassTag]( profileData: TryProfileData)(raw: K[Req, Res]) =
      raw.onEnterAndExitM(_ => timeService(), profileData.event)


    def objectify[Req: ClassTag : ToServiceRequest : ResponseCategoriser, Res: ClassTag](http: K[HttpReq, HttpRes])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseProcessor[Fail, Req, Res]) =
      toRequest ~> toHttpReq ~> http |=> toServiceResponse |=+> categoriser |=|> responseProcessor

    override protected def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parentService: K[ReqP, ResP], childService: K[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      parentService |=++> { reqP => resP => findChildIds ~+> childService |=> (seq => enricher(reqP, resP, seq)) }


    override def merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: K[Req1, Res1], secondService: K[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) =
      join2WithReq(
        reqMtoReq1 ~> firstService,
        reqMtoReq2 ~> secondService
      ) |=> merger.tupled

    override def merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: K[Req1, Res1], secondService: K[Req2, Res2], thirdService: K[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) =
      join3WithReq(
        reqMtoReq1 ~> firstService,
        reqMtoReq2 ~> secondService,
        reqMtoReq3 ~> thirdService,
      ) |=> merger.tupled

    override def merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: K[Req1, Res1], secondService: K[Req2, Res2], thirdService: K[Req3, Res3], fourthService: K[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4) =
      join4WithReq(
        reqMtoReq1 ~> firstService,
        reqMtoReq2 ~> secondService,
        reqMtoReq3 ~> thirdService,
        reqMtoReq4 ~> fourthService,
      ) |=> merger.tupled
  }

}
