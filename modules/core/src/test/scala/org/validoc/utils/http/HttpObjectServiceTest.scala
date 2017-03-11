package org.validoc.utils.http

import org.mockito.Mockito._
import org.validoc.utils.{FromServiceRequest, Service, ToServiceRequest, UtilsWithLoggingSpec}

import scala.concurrent.Future


class HttpObjectServiceTest extends UtilsWithLoggingSpec {

  behavior of "HttpObjectService with 'parser' response processor"

  case class HttpRes(s: String)

  case class HttpReq(s: String)

  type Req = String
  type Res = String

  //    override def toSummary(req: Req): String = s"summary_$req"

  implicit object ToHttpRequestForReq extends ToServiceRequest[Req] {
    override def apply(req: Req): ServiceRequest = ServiceRequest(Get, Uri(req))
  }

  implicit object FromServiceRequestForHttpReq extends FromServiceRequest[HttpReq] {
    override def apply(s: ServiceRequest): HttpReq = HttpReq(s.uri.asUriString)
  }

  def setup(serviceResponse: => ServiceResponse)(fn: (HttpObjectService[Future, HttpReq, Req, HttpRes, Res], Service[Future, HttpReq, HttpRes], ResponseProcessor[Req, Res], ServiceResponse) => Unit) = {
    val httpService = mock[Service[Future, HttpReq, HttpRes]]
    val responseProcessor = mock[ResponseProcessor[Req, Res]]
    implicit val toServiceResponse = { v1: HttpRes => serviceResponse }
    fn(new HttpObjectService("someName", httpService, responseProcessor), httpService, responseProcessor, serviceResponse)
  }

  val someHttpReq = HttpReq("http://someHttp:80")

  val someHttpRes = HttpRes("someHttpRes")
  val theServiceRequest: ServiceRequest = ToHttpRequestForReq(someHttpReq.s)

  it should "return the value from the responseProcess.statusOk when status is OK" in {
    setup(ServiceResponse(Status.Ok, Body("someResponse"), ContentType("plain/html"))) { (httpObjectService, service, responseProcessor, serviceResponse) =>

      when(service.apply(someHttpReq)) thenReturn Future.successful(someHttpRes)
      when(responseProcessor.statusOk(serviceResponse)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }

  it should "return the value from the responseProcess.statusNotFound when status is NotFound" in {
    setup(ServiceResponse(Status.NotFound, Body("someResponse"), ContentType("plain/html"))) { (httpObjectService, service, responseProcessor, serviceResponse) =>

      when(service.apply(someHttpReq)) thenReturn Future.successful(someHttpRes)
      when(responseProcessor.statusNotFound(httpObjectService.requestDetails(someHttpReq.s), serviceResponse)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }

  it should "return the value from the responseProcess.statusUnexpected when status is something unexpected" in {
    setup(ServiceResponse(Status(123), Body("someResponse"), ContentType("plain/html"))) { (httpObjectService, service, responseProcessor, serviceResponse) =>

      when(service.apply(someHttpReq)) thenReturn Future.successful(someHttpRes)
      when(responseProcessor.statusUnexpected(httpObjectService.requestDetails(someHttpReq.s), serviceResponse)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }

  it should "return the value from the responseProcess.exception when the httpService throws an exception" in {
    setup(null) { (httpObjectService, service, responseProcessor, toServiceResponse) =>
      val exception = new RuntimeException

      when(service.apply(someHttpReq)) thenReturn Future.failed(exception)
      when(responseProcessor.exception(httpObjectService.requestDetails(someHttpReq.s))(exception)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }


}
