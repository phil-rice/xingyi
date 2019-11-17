/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.endpoint

import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer
import org.mockito.Mockito._

import scala.concurrent.Future

class EndpointSpec extends UtilsSpec with ServiceRequestForEndpointFixture {

  behavior of "Endpoint"


  def setup(fn: (EndPoint[Future, String, String], MatchesServiceRequest, String => Future[String]) => Unit) = {
    val matchesServiceRequest = MatchesServiceRequest.fixedPath(Get)
    val delegate = mock[String => Future[String]]
    implicit object fromServiceRequestForString extends FromServiceRequest[Future, String] {
      override def apply(v1: ServiceRequest) = Future.successful(v1.body.map(_.asUtf).getOrElse("") + "_fromSR")
    }
    implicit def toServiceResponseForString[Req]: ToServiceResponse[Req, String] =
      req => string => ServiceResponse(Status(123), Body(string + "_toSR"), ContentType("someContentType"))


    val endPointKleisli = new EndpointKleisli[Future] with ScalaFutureAsAsyncAndMonadAndFailer
    val endPoint = endPointKleisli.endpoint[String, String]("/some/path", matchesServiceRequest)(delegate).asInstanceOf[EndPoint[Future, String, String]]
    fn(endPoint, matchesServiceRequest, delegate)
  }

  it should "have a isDefinedAt that delegates to the matchesServiceRequest" in {
    // I would use mockito but we are currying and thus that's not going to work well
    setup {
      (endpoint, matcher, delegate) =>
        endpoint.isDefinedAt(srGetPath) shouldBe true
        endpoint.isDefinedAt(srGetPathWithId) shouldBe false
    }
  }

  it should "have a apply method that returns None if the service request doesn't match" in {
    setup {
      (endpoint, matcher, delegate) =>
        await(endpoint(srGetPathWithId)) shouldBe None
    }
  }
  it should "have a apply method that calls the kleisli if the service request  matchs" in {
    setup {
      (endpoint, matcher, delegate) =>
        when(delegate.apply("someGetPathBody_fromSR")) thenReturn Future.successful("someResult")
        endpoint(srGetPath).await shouldBe Some(ServiceResponse(Status(123), Body("someResult_toSR"), ContentType("someContentType")))
    }
  }

  it should "have a toString" in {
    setup {
      (endpoint, matcher, delegate) =>
        endpoint.toString() shouldBe "Endpoint(/some/path, FixedPathAndVerb(Get))"
    }
  }

  it should "have a debugInfo for a success" in {
    setup {
      (endpoint, matcher, delegate) =>
        endpoint.debugInfo(srGetPath) shouldBe s"Endpoint(/some/path, FixedPathAndVerb(Get)) called with $srGetPath results in true"
    }
  }
  it should "have a debugInfo for a fail" in {
    setup {
      (endpoint, matcher, delegate) =>
        endpoint.debugInfo(srPutPath) shouldBe s"Endpoint(/some/path, FixedPathAndVerb(Get)) called with $srPutPath results in false"
    }
  }
}
