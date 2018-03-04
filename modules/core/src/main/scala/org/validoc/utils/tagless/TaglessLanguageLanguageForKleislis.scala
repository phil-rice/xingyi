package org.validoc.utils.tagless

import org.validoc.utils._
import org.validoc.utils.cache._
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging._
import org.validoc.utils.metrics.PutMetrics
import org.validoc.utils.objectify.ObjectifyKleisli
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.{NeedsRetry, RetryConfig, RetryService}
import org.validoc.utils.success.SuccessState
import org.validoc.utils.time.NanoTimeService

import scala.annotation.tailrec
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

trait HttpFactory[M[_], HttpReq, HttpRes] extends (ServiceName => HttpReq => M[HttpRes])

trait CommonForKleislis[M[_]] {
  type Kleisli[Req, Res] = Req => M[Res]
}

trait HttpKlesili[M[_]] extends CommonForKleislis[M] {
  protected def httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse]
  def http(name: ServiceName): ServiceRequest => M[ServiceResponse] = httpFactory(name)

}


trait MetricsKleisli[M[_], Fail] extends MetricsLanguageBase[Fail] with CommonForKleislis[M] {
  implicit def monad: MonadCanFailWithException[M, Fail]
  protected def putMetrics: PutMetrics
  protected def timeService: NanoTimeService
  def metrics[Req: ClassTag, Res: ClassTag](prefix: String)(raw: Kleisli[Req, Res])(implicit rd: RD[Res]) =
    raw.onExit[Long](_ => timeService(), (duration, mRes) => mRes.onComplete(rd.apply(prefix, duration) ~> putMetrics))

}


class TaglessLanguageLanguageForKleislis[M[_] : Async, Fail] {
  type EndpointK[Req, Res] = ServiceRequest => Option[M[ServiceResponse]]
  type Kleisli[Req, Res] = Req => M[Res]

  case class NonFunctionalLanguageService(implicit monadCanFail: MonadCanFailWithException[M, Fail],
                                          protected val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                          protected val logReqAndResult: LogRequestAndResult[M, Fail],
                                          protected val timeService: NanoTimeService,
                                          protected val putMetrics: PutMetrics,
                                          cacheFactory: CacheFactory[M],
                                          failer: Failer[Fail]) extends
    TaglessLanguage[EndpointK, Kleisli, M, Fail]
    with ObjectifyKleisli[M, Fail] with HttpKlesili[M] with MetricsKleisli[M, Fail] with LoggingKleisli[M, Fail] {

    override def monad = monadCanFail

    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Kleisli[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): EndpointK[Req, Res] = { serviceRequest: ServiceRequest => matchesServiceRequest(normalisedPath)(serviceRequest).toOption((fromServiceRequest |==> raw |=> toServiceResponse) (serviceRequest)) }
    override def chain(endpoints: EndpointK[_, _]*): Kleisli[ServiceRequest, ServiceResponse] = { req: ServiceRequest =>
      @tailrec
      def recurse(seq: List[EndpointK[_, _]]): M[ServiceResponse] = {
        seq match {
          case head :: tail =>
            head(req) match {
              case Some(result) => result
              case None => recurse(tail)
            }
          case Nil => failer.pathNotFound(req).fail
        }
      }
      recurse(endpoints.toList)
    }

    override def cache[Req: ClassTag : Cachable : ShouldCache, Res: ClassTag](name: String)(raw: Kleisli[Req, Res]): Kleisli[Req, Res] =
      Cache[M, Req, Res](cacheFactory[Req, Res](name, raw))

    override def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: Kleisli[Req, Res])(implicit retry: NeedsRetry[Fail, Res]): Kleisli[Req, Res] =
      new RetryService[M, Fail, Req, Res](raw, retryConfig)

    override def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: Kleisli[Req, Res]) =
      raw.onEnterAndExitM(_ => timeService(), profileData.event)


    override protected def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parentService: Kleisli[ReqP, ResP], childService: Kleisli[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      parentService |=++> { reqP => resP => findChildIds ~+> childService |=> (seq => enricher(reqP, resP, seq)) }


    override def merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) =
      join2WithReq(
        reqMtoReq1 ~> firstService,
        reqMtoReq2 ~> secondService
      ) |=> merger.tupled

    override def merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], thirdService: Kleisli[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) =
      join3WithReq(
        reqMtoReq1 ~> firstService,
        reqMtoReq2 ~> secondService,
        reqMtoReq3 ~> thirdService,
      ) |=> merger.tupled

    override def merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], thirdService: Kleisli[Req3, Res3], fourthService: Kleisli[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4) =
      join4WithReq(
        reqMtoReq1 ~> firstService,
        reqMtoReq2 ~> secondService,
        reqMtoReq3 ~> thirdService,
        reqMtoReq4 ~> fourthService,
      ) |=> merger.tupled
  }

}
