package org.validoc.utils.service

import org.validoc.utils.aggregate._
import org.validoc.utils.caching.{CachableKey, CachableResult, CachingService, DurationStaleCacheStategy}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.logging.LoggingService
import org.validoc.utils.map.MaxMapSizeStrategy
import org.validoc.utils.metrics._
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.profiling.ProfilingService
import org.validoc.utils.success.Succeeded
import org.validoc.utils.time.NanoTimeService
import org.validoc.utils.{FromServiceRequest, Service, ToServiceRequest, ToServiceResponse}
import scala.language.higherKinds

import scala.concurrent.duration.Duration

trait WrappedTypes[M[_]] {

  type Wrapped[Req, Res] = Service[M, Req, Res] => Service[M, Req, Res]
  type Modify[Req1, Res1, Req2, Res2] = Service[M, Req1, Res1] => Service[M, Req2, Res2]
}

trait ParserServiceBuilder[M[_], HttpReq, HttpRes] extends WrappedTypes[M] {
  protected implicit def async: Async[M]

  def parse[Req: ToServiceRequest, Res: ParserFinder](implicit toServiceResponse: ToServiceResponse[HttpRes], toHttpReq: FromServiceRequest[HttpReq]): Modify[HttpReq, HttpRes, Req, Res] =
    service => new HttpObjectService[M, HttpReq, Req, HttpRes, Res]("someName", service, ResponseProcessor.parsed(implicitly[ParserFinder[Res]]))

  def parseOption[Req: ToServiceRequest, Res: ParserFinder](implicit toServiceResponse: ToServiceResponse[HttpRes],  toHttpReq: FromServiceRequest[HttpReq]): Modify[HttpReq, HttpRes, Req, Option[Res]] =
    service => new HttpObjectService[M, HttpReq, Req, HttpRes, Option[Res]]("someName", service, ResponseProcessor.optionalParsed[Req, Res](implicitly[ParserFinder[Res]]))

  def parseDefaultIfNotFound[Req: ToServiceRequest, Res: ParserFinder](default: Res)(implicit toServiceResponse: ToServiceResponse[HttpRes],  toHttpReq: FromServiceRequest[HttpReq]): Modify[HttpReq, HttpRes, Req, Option[Res]] =
    service => new HttpObjectService[M, HttpReq, Req, HttpRes, Option[Res]]("someName", service, ResponseProcessor.optionalParsed[Req, Res](implicitly[ParserFinder[Res]]))
}

trait ServiceBuilder[M[_], HttpReq, HttpRes] extends ParserServiceBuilder[M, HttpReq, HttpRes] {

  def logging[Req, Res: Succeeded](pattern: String = "{0}"): Wrapped[Req, Res] =
    delegate => new LoggingService[M, Req, Res](delegate, pattern)

  def cache[Req: CachableKey, Res: CachableResult](maxCacheSize: Int, timeToStale: Duration, timeToDead: Duration)(implicit timeService: NanoTimeService): Wrapped[Req, Res] =
    service => new CachingService[M, Req, Res](
      "someName",
      service,
      DurationStaleCacheStategy(timeToStale.toNanos, timeToDead.toNanos),
      MaxMapSizeStrategy(maxCacheSize, Math.min(1, maxCacheSize / 5)))

  def profile[Req, Res]: Wrapped[Req, Res] = service => new ProfilingService("someName", service)


  def metrics[Req, Res: ReportData](prefix: String)(implicit timeService: NanoTimeService, putMetrics: PutMetrics, reportData: ReportData[Res]): Wrapped[Req, Res] =
    service => new MetricsService[M, Req, Res](prefix, service)(async, timeService, putMetrics, reportData)


  def aggregate[P, C](serviceP: P, serviceC: C) = (serviceP, serviceC)

  implicit class Service2Pimper[Req1, Res1, Req2, Res2](tuple: (Req1 => M[Res1], Req2 => M[Res2])) {
    def enrich[ResE](implicit enricher: Enricher[ResE, Res1, Res2], children: HasChildren[Res1, Req2]): Service[M, Req1, ResE] =
      new EnrichParentChildService[M, Req1, Res1, Req2, Res2, ResE](tuple._1, tuple._2)

    def merge[ReqE, ResE](merger: (Res1, Res2) => ResE)(implicit reqMtoReq1: ReqE => Req1, reqMtoReq2: ReqE => Req2): Service[M, ReqE, ResE] =
      new MergeService[M, ReqE, ResE, Req1, Res1, Req2, Res2](tuple._1, tuple._2, merger)
  }

}