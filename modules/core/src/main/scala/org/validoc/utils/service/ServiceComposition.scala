package org.validoc.utils.service

import org.validoc.utils.aggregate.{EnrichParentChildService, Enricher, HasChildren}
import org.validoc.utils.caching.CachingServiceLanguage
import org.validoc.utils.concurrency.Async
import org.validoc.utils.gash._
import org.validoc.utils.http._
import org.validoc.utils.logging.LoggingServiceLanguage
import org.validoc.utils.metrics._
import org.validoc.utils.profiling.ProfilingServiceLanguage
import org.validoc.utils.retry.RetryServiceLanguage

import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag

trait ServiceComposition[M[_]] {

  protected def service[OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag](serviceMakerForClass: (OldReq => M[OldRes]) => Service)
                                                                                      (implicit serviceReporter: ServiceReporter[Service]) =
    new MakeServiceDescription[M, OldReq, OldRes, Req, Res] {
      override def apply(delegate: AbstractServiceDescription[M, OldReq, OldRes]): AbstractServiceDescription[M, Req, Res] =
        DelegateServiceDescription[M, OldReq, OldRes, Req, Res, Service](delegate, serviceMakerForClass)
    }


  protected def serviceWithParam[Param, OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag]
  (param: Param, serviceMakerForClass: (Param, OldReq => M[OldRes]) => Service)(implicit serviceReporter: ServiceReporter[Service]) = new MakeServiceDescription[M, OldReq, OldRes, Req, Res] {
    override def apply(delegate: AbstractServiceDescription[M, OldReq, OldRes]): AbstractServiceDescription[M, Req, Res] =
      ParamDelegateServiceDescription[M, Param, OldReq, OldRes, Req, Res, Service](param, delegate, serviceMakerForClass)

  }
}


trait AggregateServiceLanguage[M[_]] {

  implicit class ComposePimper[OldReq, OldRes](baseDescription: AbstractServiceDescription[M, OldReq, OldRes]) {
    type OldService = OldReq => M[OldRes]

    def >-<[Req, Res](maker: MakeServiceDescription[M, OldReq, OldRes, Req, Res]): AbstractServiceDescription[M, Req, Res] = {
      maker(baseDescription)
    }

    def aggregate[ReqC, ResC, ResE](secondDescription: AbstractServiceDescription[M, ReqC, ResC]) = (baseDescription, secondDescription)

  }

  implicit class ComposeTuplePimper[Req1: ClassTag, Res1, Req2, Res2](tuple: (AbstractServiceDescription[M, Req1, Res1], AbstractServiceDescription[M, Req2, Res2])) {
    def enrich[ResE: ClassTag](implicit children: HasChildren[Res1, Req2], enricher: Enricher[ResE, Res1, Res2], async: Async[M]) =
      new MergingTwoServicesDescription[M, Req1, Res1, Req2, Res2, Req1, ResE, EnrichParentChildService[M, Req1, Res1, Req2, Res2, ResE]](tuple._1, tuple._2, {
        (old1, old2) => new EnrichParentChildService[M, Req1, Res1, Req2, Res2, ResE](old1, old2)
      })

    def merge[Req: ClassTag, ResE: ClassTag](implicit async: Async[M],
                                             merger: Merger[Res1, Res2, ResE],
                                             id1: FindId[Req, Req1],
                                             id2: FindId[Req, Req2]) =
      new MergingTwoServicesDescription[M, Req1, Res1, Req2, Res2, Req, ResE, MergingService[M, Req, ResE, Req1, Res1, Req2, Res2]](tuple._1, tuple._2, {
        (old1, old2) => new MergingService[M, Req, ResE, Req1, Res1, Req2, Res2](old1, old2)
      })
  }

}

trait ServiceCompositionLanguage[M[_]]
  extends AggregateServiceLanguage[M]
    with RetryServiceLanguage[M]
    with CachingServiceLanguage[M]
    with LoggingServiceLanguage[M]
    with ProfilingServiceLanguage[M]
    with MetricsServiceLanguage[M]
    with EndPointServiceLanguage[M]
    with DebugEndPointServiceLanguage[M]

abstract class HttpServiceCompositionLanguage[M[_] : Async, HttpReq: FromServiceRequest, HttpRes: ToServiceResponse]
(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes])
  extends ServiceCompositionLanguage[M] with HttpObjectServiceLanguage[M, HttpReq, HttpRes] with MakeHttpServiceLanguage[M, HttpReq, HttpRes] {

  def http(hostName: HostName, port: Port) = new RootHttpServiceDescription(hostName, port, makeHttpService)
}
