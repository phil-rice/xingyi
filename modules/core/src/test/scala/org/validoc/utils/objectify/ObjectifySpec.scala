package org.validoc.utils.objectify

import org.mockito.Mockito._
import org.validoc.utils.{UtilsSpec, _}
import org.validoc.utils.functions.{Async, MonadCanFail}
import org.validoc.utils.http._
import org.validoc.utils.language.Language._

import scala.concurrent.Future
import scala.language.higherKinds

trait ServiceResponseFixture {
  val serviceRequest = ServiceRequest(Get, Uri("/someUri"))
  val serviceResponse = ServiceResponse(Status(1), Body("someBody"), ContentType("something"))
  val serviceResponse2 = ServiceResponse(Status(1), Body("someBody2"), ContentType("somethingElse"))
  val reqAndServiceResponse = RequestAndServiceResponse("input", serviceResponse2)

}
class AbstractObjectifySpec[M[_] : Async, Fail](implicit val monad: MonadCanFail[M, Fail]) extends UtilsSpec with ObjectifyKleisli[M, Fail] with FunctionFixture with ServiceResponseFixture {

  behavior of "Objectify"
  type ServiceKleisli = ServiceRequest => M[ServiceResponse]
  type StringKleisli = String => M[String]

  def setup[X](fn: (StringKleisli, ToServiceRequest[String], ServiceKleisli, ResponseCategoriser[M, String], ResponseParser[Fail, String, String]) => X): X = {
    implicit val toRequest = mock[ToServiceRequest[String]]
    implicit val service = mock[ServiceKleisli]
    implicit val categoriser: ResponseCategoriser[M, String] = new ResponseCategoriser[M, String] {
      //      override def apply(v1: String) =
      override def apply(v1: String) = {
        fn2Curried("input", serviceResponse, reqAndServiceResponse.liftM)(v1)
      }
    }
    implicit val responseParser = mock[ResponseParser[Fail, String, String]]
    fn(objectify(service), toRequest, service, categoriser, responseParser)
  }
  it should "turn a request into a service request, present it to the http service, categorise the result and process it returning a response" in {
    setup { (kleisli, toService, service, categoriser, parser) =>
      when(toService.apply("input")) thenReturn (serviceRequest)
      when(service.apply(serviceRequest)) thenReturn (serviceResponse.liftM)
      when(parser.apply(reqAndServiceResponse)) thenReturn Right("output")
      kleisli("input").await() shouldBe "output"
    }
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

class FutureObjectifySpec extends AbstractObjectifySpec[Future, Throwable]