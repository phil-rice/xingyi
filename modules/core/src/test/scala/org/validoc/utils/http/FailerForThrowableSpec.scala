package org.validoc.utils.http

import org.validoc.utils.UtilsSpec
import org.validoc.utils.exceptions.{EndpointNotFoundException, NotFoundException, UnexpectedStatusCodeException}

class FailerForThrowableSpec extends UtilsSpec {


  val failer = implicitly[Failer[Throwable]]
  val serviceRequest = ServiceRequest(Get, Uri("/someUri"))
  val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("someContentType"))
  behavior of "FailerForThrowable"

  it should "throw a NotFoundException when notFound" in {
    val e = intercept[NotFoundException](failer.notFound("someReq", serviceResponse))
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "throw a UnexpectedStatusCodeException when unexpected statusCode" in {
    val e = intercept[UnexpectedStatusCodeException](failer.unexpected("someReq", serviceResponse))
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "throw a EndpointNotFoundException when pathNotFound" in {
    val e = intercept[EndpointNotFoundException](failer.pathNotFound(serviceRequest))
    e.serviceRequest shouldBe serviceRequest
  }

}
