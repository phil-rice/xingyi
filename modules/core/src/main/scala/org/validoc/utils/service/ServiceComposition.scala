package org.validoc.utils.service

import org.validoc.utils.Parser
import org.validoc.utils.aggregate.{EnrichParentChildService, Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, CachableResult, CachingService, CachingServiceLanguage}
import org.validoc.utils.concurrency.{Async, MDCPropagatingExecutionContext}
import org.validoc.utils.gash._
import org.validoc.utils.http._
import org.validoc.utils.logging.{LoggingService, LoggingServiceLanguage, NullLoggingAdapterWithMdc}
import org.validoc.utils.metrics._
import org.validoc.utils.parser.{ParserFinder, ParserResult}
import org.validoc.utils.profiling.{ProfilingService, ProfilingServiceLanguage}
import org.validoc.utils.retry.{NeedsRetry, RetryConfig, RetryService, RetryServiceLanguage}
import org.validoc.utils.success.Succeeded
import org.validoc.utils.time.{NanoTimeService, RandomDelay}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag
import scala.util.Try

trait ServiceComposition {

  implicit class ComposePimper[M[_], OldReq, OldRes](baseDescription: ServiceDescription[M, OldReq, OldRes]) {
    type OldService = OldReq => M[OldRes]

    def >-<[Req, Res](maker: MakeServiceDescription[M, OldReq, OldRes, Req, Res]): ServiceDescription[M, Req, Res] = {
      maker(baseDescription)
    }

    def aggregate[ReqC, ResC, ResE](secondDescription: ServiceDescription[M, ReqC, ResC]) = (baseDescription, secondDescription)

  }

  implicit class ComposeTuplePimper[M[_] : Async, Req1, Res1, Req2, Res2](tuple: (ServiceDescription[M, Req1, Res1], ServiceDescription[M, Req2, Res2])) {
    def enrich[ResE](implicit children: HasChildren[Res1, Req2], enricher: Enricher[ResE, Res1, Res2]) = {
      //                    (implicit makeServiceMakerForTwoServices: MakeServiceMakerForTwoServices[Req1 => M[Res1], Req2 => M[Res2], EnrichParentChildService[M, Req1, Res1, Req2, Res2, ResE]]) = {
      //      val x: MakeServiceMakerForTwoServices[(Req1) => M[Res1], (Req2) => M[Res2], EnrichParentChildService[M, Req1, Res1, Req2, Res2, ResE]]
      new MergingTwoServicesDescription[M, Req1, Res1, Req2, Res2, Req1, ResE, EnrichParentChildService[M, Req1, Res1, Req2, Res2, ResE]](tuple._1, tuple._2)
    }

    def merge[Req, Res](implicit merger: Merger[Res1, Res2, Res],
                        id1: FindId[Req, Req1],
                        id2: FindId[Req, Req2]) = {
      new MergingTwoServicesDescription[M, Req1, Res1, Req2, Res2, Req, Res, MergingService[M, Req, Res, Req1, Res1, Req2, Res2]](tuple._1, tuple._2)
    }

  }

  protected def serviceDescription[M[_], OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag]
  (implicit serviceMakerForClass: MakeServiceMakerForClass[OldReq => M[OldRes], Service], serviceReporter: ServiceReporter[Service]) =
    new MakeServiceDescription[M, OldReq, OldRes, Req, Res] {
      override def apply(delegate: ServiceDescription[M, OldReq, OldRes]): ServiceDescription[M, Req, Res] =
        DelegateServiceDescription[M, OldReq, OldRes, Req, Res, Service](delegate, serviceMakerForClass)
    }

  protected def serviceDescription2[M[_], OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag](serviceMakerForClass: (OldReq => M[OldRes]) => Service)
                                                                                                        (implicit serviceReporter: ServiceReporter[Service]) =
    new MakeServiceDescription[M, OldReq, OldRes, Req, Res] {
      override def apply(delegate: ServiceDescription[M, OldReq, OldRes]): ServiceDescription[M, Req, Res] =
        DelegateServiceDescription[M, OldReq, OldRes, Req, Res, Service](delegate, serviceMakerForClass)
    }

  protected def serviceDescriptionWithParam[M[_], Param, OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag]
  (param: Param)
  (implicit serviceMakerForClass: MakeServiceMakerForClassWithParam[Param, OldReq => M[OldRes], Service], serviceReporter: ServiceReporter[Service])
  = new MakeServiceDescription[M, OldReq, OldRes, Req, Res] {
    override def apply(delegate: ServiceDescription[M, OldReq, OldRes]): ServiceDescription[M, Req, Res] =
      ParamDelegateServiceDescription[M, Param, OldReq, OldRes, Req, Res, Service](param, delegate, serviceMakerForClass)

  }

  protected def serviceDescriptionWithParam2[M[_], Param, OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag]
  (param: Param, serviceMakerForClass: (Param, OldReq => M[OldRes]) => Service)(implicit serviceReporter: ServiceReporter[Service])
  = new MakeServiceDescription[M, OldReq, OldRes, Req, Res] {
    override def apply(delegate: ServiceDescription[M, OldReq, OldRes]): ServiceDescription[M, Req, Res] =
      ParamDelegateServiceDescription[M, Param, OldReq, OldRes, Req, Res, Service](param, delegate, serviceMakerForClass)

  }
}

trait ServiceCompositionLanguage extends ServiceComposition
  with RetryServiceLanguage
  with CachingServiceLanguage
  with LoggingServiceLanguage
  with ProfilingServiceLanguage
  with MetricsServiceLanguage
  with EndPointServiceLanguage

abstract class HttpServiceCompositionLanguage[M[_] : Async, HttpReq: FromServiceRequest, HttpRes: ToServiceResponse](implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes])
  extends ServiceComposition with ServiceCompositionLanguage {


  def http(hostName: HostName, port: Port) = new RootHttpServiceDescription(hostName, port, makeHttpService)

  def asObject[Req: ToServiceRequest, Res: ParserFinder] =
    serviceDescription[M, HttpReq, HttpRes, Req, Res, HttpObjectService[M, HttpReq, Req, HttpRes, Res]]

}

object ExampleComposition {
  implicit val loggingAdapter = NullLoggingAdapterWithMdc
  implicit val ec: MDCPropagatingExecutionContext = ExecutionContext.global
  implicit val reportData = new ReportData[String] {
    override def apply(v1: String, v2: Try[String], v3: Long): Map[String, MetricValue] = ???
  }

  implicit object serviceRequestDefault extends ToServiceRequest[Int] {
    override def apply(v1: Int): ServiceRequest = ???
  }

  implicit object fromServiceREquest extends FromServiceRequest[Int] {
    override def apply(v1: ServiceRequest): Int = ???
  }

  implicit val parserFinder = new ParserFinder[String] {
    override def find(contentType: ContentType): ParserResult[Parser[String]] = ???
  }

  implicit object ToServiceResponseForString extends ToServiceResponse[String] {
    override def apply(v1: String): ServiceResponse = ServiceResponse(Status.Ok, Body(v1), ContentType("text/plain"))
  }

  implicit val makeHttpService: MakeHttpService[Future, Int, String] = ???

  class ExampleComposition extends HttpServiceCompositionLanguage[Future, Int, String] {


    val z = http(HostName("localhost"), Port(9000)) >-< asObject[Int, String] >-< cache >-< log("somepattern{0}") >-< profile >-< metrics("somePrefix") >-< retry(RetryConfig(1, new RandomDelay(1 second)))

  }

}