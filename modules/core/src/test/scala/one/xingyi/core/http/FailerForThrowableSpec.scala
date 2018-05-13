package one.xingyi.core.http

import one.xingyi.core.UtilsSpec
import one.xingyi.core.exceptions.{EndpointNotFoundException, NotFoundException, UnexpectedStatusCodeException}
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer

class FailerForThrowableSpec extends UtilsSpec with ScalaFutureAsAsyncAndMonadAndFailer {


  val serviceRequest = ServiceRequest(Get, Uri("/someUri"))
  val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("someContentType"))
  behavior of "FailerForThrowable"

  it should "Future.exception a NotFoundException when notFound" in {
    val m = failer.notFound("someReq", serviceResponse)
    val e = m.asInstanceOf[NotFoundException]
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "Future.exception a UnexpectedStatusCodeException when unexpected statusCode" in {
    val m = failer.unexpected("someReq", serviceResponse)
    val e = m.asInstanceOf[UnexpectedStatusCodeException]
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "Future.exception a EndpointNotFoundException when pathNotFound" in {
    val m = failer.pathNotFound(serviceRequest)
    val e = m.asInstanceOf[EndpointNotFoundException]
    e.serviceRequest shouldBe serviceRequest
  }

}
