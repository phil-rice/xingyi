package org.validoc.utils.service

import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.metrics.{NullPutMetrics, PutMetrics}
import org.validoc.utils.success.Succeeded
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.language.implicitConversions

class ServerContext[HttpReq:FromServiceRequest, HttpRes:ToServiceResponse](implicit val timeService: NanoTimeService,
                                      val putMetrics: PutMetrics,
                                      val succeeded: Succeeded[HttpRes],
                                      val toServiceRequest: ToServiceRequest[HttpReq],
                                      val toServiceResponse: ToServiceResponse[HttpRes],
                                      val fromServiceRequest: FromServiceRequest[HttpReq]) {
}

object ServerContext {
  implicit val serverContextForStrings = {
    implicit def toJsonForString = new ToJson[String] {
      override def apply(v1: String): String = v1
    }

    implicit def toServiceResponceForString = new ToServiceResponse[String] {
      override def apply(v1: String): ServiceResponse = ServiceResponse(Status.Ok, Body(v1), ContentType("text/plain"))
    }

    implicit def toServiceRequestforString = new ToServiceRequest[String] {
      override def apply(v1: String): ServiceRequest = ServiceRequest(Get, Uri(v1))
    }

    implicit def fromServiceRequestorString = new FromServiceRequest[String] {
      override def apply(v1: ServiceRequest): String = v1.uri.asUriString
    }


    implicit val nanoTimeService = SystemClockNanoTimeService

    implicit val putMetrics = NullPutMetrics

    new ServerContext[String, String]()
  }
}


class ServerContextForMocks(implicit timeService: NanoTimeService, succeeded: Succeeded[ServiceResponse], putMetrics: PutMetrics = NullPutMetrics)
  extends ServerContext[ServiceRequest, ServiceResponse]


