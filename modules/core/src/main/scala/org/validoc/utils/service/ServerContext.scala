package org.validoc.utils.service

import org.validoc.utils.http._
import org.validoc.utils.metrics.{NullPutMetrics, PutMetrics}
import org.validoc.utils.success.Succeeded
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}
import org.validoc.utils.{FromServiceRequest, ToServiceRequest, ToServiceResponse}

import scala.language.implicitConversions

class ServerContext[HttpReq, HttpRes](implicit val timeService: NanoTimeService,
                                      val putMetrics: PutMetrics,
                                      val succeeded: Succeeded[HttpRes],
                                      val toServiceRequest: ToServiceRequest[HttpReq],
                                      val toServiceResponse: ToServiceResponse[HttpRes],
                                      val fromServiceRequest: FromServiceRequest[HttpReq]) {
}

object ServerContext{
  implicit val  serverContextForStrings ={
    implicit def serviceResponseForString(v1: String) = ServiceResponse(Status.Ok, Body(v1), ContentType("text/plain"))

    implicit def stringToServiceRequest(v1: String) = ServiceRequest(Get, Uri(v1))

    implicit def serviceRequestToString(v1: ServiceRequest): String = v1.uri.asUriString

    implicit val nanoTimeService = SystemClockNanoTimeService

    implicit val putMetrics = NullPutMetrics

    new ServerContext[String, String]()
  }
}



class ServerContextForMocks(timeService: NanoTimeService, succeeded: Succeeded[ServiceResponse])
  extends ServerContext[ServiceRequest, ServiceResponse]()(timeService, NullPutMetrics, succeeded, identity[ServiceRequest], identity[ServiceResponse], identity[ServiceRequest])


