package org.validoc.utils.objectify

import org.mockito.Mockito._
import org.validoc.utils.{UtilsSpec, _}
import org.validoc.utils.functions.{Async, MonadCanFail}
import org.validoc.utils.http._
import org.validoc.utils.language.Language._
import org.validoc.utils.logging.DetailedLogging

import scala.concurrent.Future
import scala.language.higherKinds

trait ServiceResponseFixture {
  val serviceRequest = ServiceRequest(Get, Uri("/someUri"))
  val serviceResponse = ServiceResponse(Status(1), Body("someBody"), ContentType("something"))
  val serviceResponse2 = ServiceResponse(Status(1), Body("someBody2"), ContentType("somethingElse"))
  val reqAndServiceResponse = RequestAndServiceResponse("input", serviceResponse2)

}

class AbstractObjectifySpec[M[_] : Async, Fail](implicit protected val monad: MonadCanFail[M, Fail],
                                                protected val failer: Failer[Fail],
                                                protected val detailsLoggingForSR: DetailedLogging[ServiceResponse]) extends UtilsSpec with ObjectifyKleisli[M, Fail] with FunctionFixture with ServiceResponseFixture {

  behavior of "Objectify"
  type ServiceKleisli = ServiceRequest => M[ServiceResponse]
  type StringKleisli = String => M[String]

  def setup[X](fn: (StringKleisli, ToServiceRequest[String], ServiceKleisli, ResponseCategoriser[String], ResponseParser[String, String]) => X): X = {
    implicit val toRequest = mock[ToServiceRequest[String]]
    implicit val service = mock[ServiceKleisli]
    implicit val categoriser: ResponseCategoriser[String] = new ResponseCategoriser[String] {
      override def categorise[Fail](implicit failer: Failer[Fail]): String => ServiceResponse => Either[Fail, RequestAndServiceResponse[String]] =
        fn2Curried("input", serviceResponse, Right(reqAndServiceResponse))

    }
    implicit val responseParser: ResponseParser[String, String] = new ResponseParser[String, String] {
      override def parse[Fail](actualRequestAndServiceResponse: RequestAndServiceResponse[String])(implicit failer: ResponseParserFailer[Fail], reqDetails: DetailedLogging[String], srDetails: DetailedLogging[ServiceResponse]): Either[Fail, String] = {
        actualRequestAndServiceResponse shouldBe reqAndServiceResponse
        Right("output")
      }
    }


    fn(objectify(service), toRequest, service, categoriser, responseParser)
  }
  it should "turn a request into a service request, present it to the http service, categorise the result and process it returning a response" in {
    setup { (kleisli, toService, service, categoriser, parser) =>
      when(toService.apply("input")) thenReturn serviceRequest
      when(service.apply(serviceRequest)) thenReturn serviceResponse.liftM
      kleisli("input").await() shouldBe "output"

      //      toRequest ~> http |==+> categoriser |=|> responseProcessor.parse[Fail]

    }
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._
import Failer.failerForThrowable

class FutureObjectifySpec extends AbstractObjectifySpec[Future, Throwable]