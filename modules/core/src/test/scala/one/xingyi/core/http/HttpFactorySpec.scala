package one.xingyi.core.http

import one.xingyi.core.UtilsSpec

import scala.concurrent.Future

import org.mockito.Mockito._

class HttpFactorySpec extends UtilsSpec {

  behavior of "HttpKleisli"

  val serviceName = ServiceName("someName")
  val httpService = mock[ServiceRequest => Future[ServiceResponse]]

  it should "get the http kleisli from the factory" in {
    val mockHttpFactory = mock[HttpFactory[Future, ServiceRequest, ServiceResponse]]
    val httpKlesili = new HttpKlesili[Future] {
      override protected def httpFactory: HttpFactory[Future, ServiceRequest, ServiceResponse] = mockHttpFactory
    }
    when(mockHttpFactory.apply(serviceName)) thenReturn (httpService)
    httpKlesili.http(serviceName) shouldBe httpService

  }

}
