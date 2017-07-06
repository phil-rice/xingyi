package org.validoc.utils.caching

import org.validoc.utils.service.{AbstractServiceDescription, RootServiceDescription, ServiceDescription, ServiceReporter}
import org.validoc.utils.{UtilsSpec, UtilsWithExecutionContextSpec}

import scala.concurrent.Future
import org.mockito.Mockito._

class CachingLanguageSpec extends UtilsWithExecutionContextSpec with CachingServiceLanguage[Future] {


  behavior of "CachingLanguage"


  type SD = AbstractServiceDescription[Future, Int, String]
  type CS = CachingService[Future, Int, String]
  type RAW = Int => Future[String]

  def withMocks(fn: SD => CS => RAW => ServiceReporter[CS] => Unit): Unit = {
    val delegate = mock[Int => Future[String]]
    val root = RootServiceDescription[Future, String, Int, String]("ignored", _ => delegate)
    implicit val serviceReporter = mock[ServiceReporter[CachingService[Future, Int, String]]]
    val sd = root >-< cache
    val cachingService = sd.service.asInstanceOf[CachingService[Future, Int, String]]
    fn(sd)(cachingService)(delegate)(serviceReporter)
  }

  it should "allow a cache to be created with the delegate" in {
    withMocks { serviceDescription: SD => cachingService: CS => delegate: RAW => serviceReporter => cachingService.delegate shouldBe delegate }
  }
  it should "pull in a serviceReporter if one exist" in {
    withMocks { serviceDescription: SD =>
      cachingService: CS =>
        delegate: RAW =>
          serviceReporter =>
            val captor = captorFor[CachingService[Future, Int, String]]
            when(serviceReporter.apply(captor.capture())) thenReturn (Some("value"))
            serviceDescription.report shouldBe Some("value")
    }
  }
}
