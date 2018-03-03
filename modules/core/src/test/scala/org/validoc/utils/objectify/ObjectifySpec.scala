package org.validoc.utils.objectify

import org.validoc.utils.UtilsSpec
import org.validoc.utils.functions.{Async, Monad}
import org.validoc.utils.http._

import scala.concurrent.Future
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.mockito.Mockito._
import org.validoc.utils._

class AbstractObjectifySpec[M[_] : Async](implicit val monad: Monad[M]) extends UtilsSpec with ObjectifyKleisli[M] with FunctionFixture {

  behavior of "Objectify"
  type ServiceKleisli = Kleisli[ServiceRequest, ServiceResponse]
  type StringKleisli = Kleisli[String, String]
  val serviceRequest = ServiceRequest(Get, Uri("/someUri"))
  val serviceResponse = ServiceResponse(Status(1), Body("someBody"), ContentType("something"))
  val serviceResponse2 = ServiceResponse(Status(1), Body("someBody2"), ContentType("somethingElse"))
  val responseOK = ResponseOk("something", serviceResponse2)

  def setup[X](fn: (StringKleisli, ToServiceRequest[String], ServiceKleisli, ResponseCategoriser[String], ResponseProcessor[M, String, String]) => X): X = {
    implicit val toRequest = mock[ToServiceRequest[String]]
    implicit val service = mock[ServiceKleisli]
    implicit val categoriser: ResponseCategoriser[String] = new ResponseCategoriser[String] {
      override def apply(v1: String) = fn2Curried("input", serviceResponse, responseOK)(v1)
    }
    implicit val responseProcessor = mock[ResponseProcessor[M, String, String]]
    fn(objectify(service), toRequest, service, categoriser, responseProcessor)
  }
  it should "turn a request into a service request, present it to the http service, categorise the result and process it returning a response" in {
    setup { (kleisli, toService, service, categoriser, responseProcessor) =>
      when(toService.apply("input")) thenReturn (serviceRequest)
      when(service.apply(serviceRequest)) thenReturn (serviceResponse.liftM)
      when(responseProcessor.apply(responseOK)) thenReturn "output".liftM
      kleisli("input").await() shouldBe "output"
    }
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._

class FutureObjectifySpec extends AbstractObjectifySpec[Future]