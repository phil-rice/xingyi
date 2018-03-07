package org.validoc.utils.http

import org.validoc.utils.UtilsSpec
import org.validoc.utils.exceptions.{EndpointNotFoundException, NotFoundException, UnexpectedStatusCodeException}
import org.validoc.utils.functions.ScalaFutureAsAsyncAndMonadAndFailer

import scala.concurrent.Future

class FailerForThrowableSpec extends UtilsSpec with ScalaFutureAsAsyncAndMonadAndFailer {


  val serviceRequest = ServiceRequest(Get, Uri("/someUri"))
  val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("someContentType"))
  behavior of "FailerForThrowable"

  it should "Future.exception a NotFoundException when notFound" in {
    val m = failer.notFound("someReq", serviceResponse)
    val e = intercept[NotFoundException](await(m))
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "Future.exception a UnexpectedStatusCodeException when unexpected statusCode" in {
    val m = failer.unexpected("someReq", serviceResponse)
    val e = intercept[UnexpectedStatusCodeException](await(m))
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "Future.exception a EndpointNotFoundException when pathNotFound" in {
    val m = failer.pathNotFound(serviceRequest)
    val e = intercept[EndpointNotFoundException](await(m))
    e.serviceRequest shouldBe serviceRequest
  }

}
