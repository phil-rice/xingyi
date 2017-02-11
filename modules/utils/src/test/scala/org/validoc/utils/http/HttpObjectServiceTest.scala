package org.validoc.utils.http

import java.util.concurrent.Executors

import org.validoc.utils.{Service, UtilsSpec}
import org.validoc.utils.concurrency.Async
import org.mockito.Mockito._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success


class HttpObjectServiceTest extends UtilsSpec {
  behavior of "HttpObjectService with 'parser' response processor"

  case class HttpRes(s: String)

  case class HttpReq(s: String)

  type Req = String
  type Res = String
  //    override def toSummary(req: Req): String = s"summary_$req"
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  implicit object ToHttpRequestForReq extends ToRequest[Req] {
    override def toRequest(req: Req): ServiceRequest = ServiceRequest(Get, Uri(req))
  }

  implicit def convert(s: ServiceRequest): HttpReq = HttpReq(s.uri.asUriString)

  def setup(fn: (HttpObjectService[Future, HttpReq, Req, HttpRes, Res], Service[Future, HttpReq, HttpRes], ResponseProcessor[Req, Res], ToServiceResponse[HttpRes]) => Unit) = {
    val httpService = mock[Service[Future, HttpReq, HttpRes]]
    val responseProcessor = mock[ResponseProcessor[Req, Res]]
    implicit val toServiceResponse = mock[ToServiceResponse[HttpRes]]
    fn(new HttpObjectService("someName", httpService, responseProcessor), httpService, responseProcessor, toServiceResponse)
  }

  val someHttpReq = HttpReq("http://someHttp:80")

  val someHttpRes = HttpRes("someHttpRes")
  val theServiceRequest: ServiceRequest = ToHttpRequestForReq.toRequest(someHttpReq.s)

  it should "return the value from the responseProcess.statusOk when status is OK" in {
    setup { (httpObjectService, service, responseProcessor, toServiceResponse) =>
      val serviceResponse = ServiceResponse(Status.Ok, Body("someResponse"), ContentType("plain/html"))

      when(service.apply(someHttpReq)) thenReturn Future.successful(someHttpRes)
      when(toServiceResponse.response(someHttpRes)) thenReturn serviceResponse
      when(responseProcessor.statusOk(serviceResponse)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }

  it should "return the value from the responseProcess.statusNotFound when status is NotFound" in {
    setup { (httpObjectService, service, responseProcessor, toServiceResponse) =>
      val serviceResponse = ServiceResponse(Status.NotFound, Body("someResponse"), ContentType("plain/html"))

      when(service.apply(someHttpReq)) thenReturn Future.successful(someHttpRes)
      when(toServiceResponse.response(someHttpRes)) thenReturn serviceResponse
      when(responseProcessor.statusNotFound(httpObjectService.requestDetails(someHttpReq.s), serviceResponse)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }

  it should "return the value from the responseProcess.statusUnexpected when status is something unexpected" in {
    setup { (httpObjectService, service, responseProcessor, toServiceResponse) =>
      val serviceResponse = ServiceResponse(Status(123), Body("someResponse"), ContentType("plain/html"))

      when(service.apply(someHttpReq)) thenReturn Future.successful(someHttpRes)
      when(toServiceResponse.response(someHttpRes)) thenReturn serviceResponse
      when(responseProcessor.statusUnexpected(httpObjectService.requestDetails(someHttpReq.s), serviceResponse)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }

  it should "return the value from the responseProcess.exception when the httpService throws an exception" in {
    setup { (httpObjectService, service, responseProcessor, toServiceResponse) =>
      val exception = new RuntimeException

      when(service.apply(someHttpReq)) thenReturn Future.failed(exception)
      when(responseProcessor.exception(httpObjectService.requestDetails(someHttpReq.s), exception)) thenReturn "result"

      await(httpObjectService(someHttpReq.s)) shouldBe "result"
    }
  }


}
