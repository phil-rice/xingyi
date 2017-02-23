package org.validoc.utils.http

import java.util.concurrent.Executors

import org.mockito.Mockito._
import org.validoc.utils.concurrency.MDCPropagatingExecutionContext
import org.validoc.utils.{Service, UtilsSpec}

import scala.concurrent.{ExecutionContext, Future}


class HttpObjectServiceTest extends UtilsSpec {

  behavior of "HttpObjectService with 'parser' response processor"

  case class HttpRes(s: String)

  case class HttpReq(s: String)

  type Req = String
  type Res = String
  //    override def toSummary(req: Req): String = s"summary_$req"

  implicit object ToHttpRequestForReq extends ToServiceRequest[Req] {
    override def apply(req: Req): ServiceRequest = ServiceRequest(Get, Uri(req))
  }

  implicit def convert(s: ServiceRequest): HttpReq = HttpReq(s.uri.asUriString)

  def setup(serviceResponse: => ServiceResponse)(fn: (HttpObjectService[Future, HttpReq, Req, HttpRes, Res], Service[Future, HttpReq, HttpRes], ResponseProcessor[Req, Res], ServiceResponse) => Unit) = {
    val httpService = mock[Service[Future, HttpReq, HttpRes]]
    val responseProcessor = mock[ResponseProcessor[Req, Res]]
    implicit val toServiceResponse = new ToServiceResponse[HttpRes] {
      override def apply(v1: HttpRes): ServiceResponse = serviceResponse
    }
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
      when(responseProcessor.exception(httpObjectService.requestDetails(someHttpReq.s), exception)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }


}
