/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.objectify

import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.logging.DetailedLogging
import one.xingyi.core.monad.{Async, MonadCanFail}
import one.xingyi.core.{UtilsSpec, _}
import org.mockito.Mockito._

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
                                                protected val detailedLoggingForSR: DetailedLogging[ServiceResponse]) extends UtilsSpec with ObjectifyKleisli[M, Fail] with FunctionFixture with ServiceResponseFixture {

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

import one.xingyi.core.http.Failer.failerForThrowable
import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class FutureObjectifySpec extends AbstractObjectifySpec[Future, Throwable]
