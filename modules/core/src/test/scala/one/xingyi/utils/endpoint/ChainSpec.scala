package one.xingyi.utils.endpoint

import one.xingyi.utils.UtilsSpec
import one.xingyi.utils.functions.{MonadCanFail, ScalaFutureAsAsyncAndMonadAndFailer}
import one.xingyi.utils.http._

import scala.concurrent.Future
import org.mockito.Mockito._
import one.xingyi.utils.exceptions.EndpointNotFoundException

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
