package org.validoc.utils.endpoint

import org.validoc.utils.UtilsSpec
import org.validoc.utils.functions.{MonadCanFail, ScalaFutureAsAsyncAndMonadAndFailer}
import org.validoc.utils.http._

import scala.concurrent.Future
import org.mockito.Mockito._
import org.validoc.utils.exceptions.EndpointNotFoundException

class ChainSpec extends UtilsSpec with ServiceRequestForEndpointFixture {

  behavior of "ChainingKleisli.chain"


  def setup(fn: ((ServiceRequest => Future[ServiceResponse]), EndPoint[Future, Int, String], EndPoint[Future, String, Int], EndPoint[Future, String, Double]) => Unit): Unit = {
    val chainKleisli = new ChainKleisli[Future, Throwable] with ScalaFutureAsAsyncAndMonadAndFailer
    val endPoint1 = mock[EndPoint[Future, Int, String]]
    val endPoint2 = mock[EndPoint[Future, String, Int]]
    val endPoint3 = mock[EndPoint[Future, String, Double]]
    val chain: ServiceRequest => Future[ServiceResponse] = chainKleisli.chain(endPoint1, endPoint2, endPoint3)
    fn(chain, endPoint1, endPoint2, endPoint3)
  }
  it should "  throw (in future) EndpointNotFoundException if none match" in {
    setup { (chain, ep1, ep2, ep3) =>
      when(ep1.isDefinedAt(srGetPath)) thenReturn false
      when(ep2.isDefinedAt(srGetPath)) thenReturn false
      when(ep3.isDefinedAt(srGetPath)) thenReturn false

      val result = chain(srGetPath)
      intercept[EndpointNotFoundException](await(result))

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
      when(ep2.apply(srGetPath)) thenReturn Future.successful(serviceResponse)
      when(ep3.isDefinedAt(srGetPath)) thenReturn false

      await(chain(srGetPath)) shouldBe serviceResponse

      verify(ep1, times(0)).apply(srGetPath)
      verify(ep2, times(1)).apply(srGetPath)
      verify(ep3, times(0)).apply(srGetPath)
    }
  }
}
