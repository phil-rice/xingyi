/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.endpoint

import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer
import org.mockito.Mockito._

import scala.concurrent.Future

class ChainSpec extends UtilsSpec with ServiceRequestForEndpointFixture {

  behavior of "ChainingKleisli.chain"


  def setup(fn: ((ServiceRequest => Future[Option[ServiceResponse]]), EndPoint[Future, Int, String], EndPoint[Future, String, Int], EndPoint[Future, String, Double]) => Unit): Unit = {
    val chainKleisli = new ChainKleisli[Future, Throwable] with ScalaFutureAsAsyncAndMonadAndFailer
    val endPoint1 = mock[EndPoint[Future, Int, String]]
    val endPoint2 = mock[EndPoint[Future, String, Int]]
    val endPoint3 = mock[EndPoint[Future, String, Double]]
    val chain: ServiceRequest => Future[Option[ServiceResponse]] = chainKleisli.chain(endPoint1, endPoint2, endPoint3)
    fn(chain, endPoint1, endPoint2, endPoint3)
  }

  it should "return a future of none if doesn't match" in {
    setup { (chain, ep1, ep2, ep3) =>
      when(ep1.isDefinedAt(srGetPath)) thenReturn false
      when(ep2.isDefinedAt(srGetPath)) thenReturn false
      when(ep3.isDefinedAt(srGetPath)) thenReturn false

      val result = chain(srGetPath)
      await(result) shouldBe None

      verify(ep1, times(0)).apply(srGetPath)
      verify(ep2, times(0)).apply(srGetPath)
      verify(ep3, times(0)).apply(srGetPath)
    }
  }

  it should "pass the service request to the first endpoint that matches" in {
    setup { (chain, ep1, ep2, ep3) =>
      val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("someContent"))
      when(ep1.isDefinedAt(srGetPath)) thenReturn false
      when(ep2.isDefinedAt(srGetPath)) thenReturn true
      when(ep2.apply(srGetPath)) thenReturn Future.successful(Some(serviceResponse))
      when(ep3.isDefinedAt(srGetPath)) thenReturn false

      await(chain(srGetPath)) shouldBe Some(serviceResponse)

      verify(ep1, times(0)).apply(srGetPath)
      verify(ep2, times(1)).apply(srGetPath)
      verify(ep3, times(0)).apply(srGetPath)
    }
  }
}
