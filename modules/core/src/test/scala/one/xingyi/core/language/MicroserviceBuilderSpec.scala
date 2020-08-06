/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language

import java.text.MessageFormat

import one.xingyi.core.cache._
import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.logging._
import one.xingyi.core.metrics.{CountMetricValue, MetricValue, PutMetrics, ReportData}
import one.xingyi.core.monad.{Async, IdentityMonad, Liftable, MonadCanFailWithException}
import one.xingyi.core.profiling.{ProfileService, TryProfileData}
import one.xingyi.core.retry.{RetryConfig, RetryService}
import one.xingyi.core.time.{MockTimeService, NanoTimeService, RandomDelay}
import one.xingyi.core.{FunctionFixture, MockitoSugar, UtilsSpec}
import org.mockito.Mockito._

import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class MicroserviceBuilderForTest[M[_], Fail](implicit val async: Async[M], val monad: MonadCanFailWithException[M, Fail], val detailedLoggingForSR: DetailedLogging[ServiceResponse]) extends MicroserviceBuilder[M, Fail] with MockitoSugar {
  override val cacheFactory: CacheFactory[M] = mock[CacheFactory[M]]
  override implicit val timeService: NanoTimeService = new MockTimeService
  implicit val log = new RememberLoggingAdapter
  override val logReqAndResult: LogRequestAndResult[Fail] = new AbstractLogRequestAndResult[Fail]() {
    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*): String =
      MessageFormat.format(messagePrefix, strings: _*)
  }
  override val failer: Failer[Fail] = mock[Failer[Fail]]
  override val putMetrics: PutMetrics = mock[PutMetrics]
  override val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse] = mock[HttpFactory[M, ServiceRequest, ServiceResponse]]
}

case class ThingRequest(id: String)
object ThingRequest {
  implicit def fromServiceRequest: ToServiceRequest[ThingRequest] = thingy => ServiceRequest(Method("get"), Uri("/" + thingy.id))
  implicit def fromServiceRequest[M[_] : Liftable]: FromServiceRequest[M, ThingRequest] = sr => ThingRequest(sr.uri.asUriString).liftM
}
case class ThingResponse(id: String, response: String)
object ThingResponse {
  implicit def responseProcess: ResponseParser[ThingRequest, ThingResponse] = new ResponseParser[ThingRequest, ThingResponse] {
    override def parse[Fail](requestAndServiceResponse: RequestAndServiceResponse[ThingRequest])(
      implicit failer: ResponseParserFailer[Fail], reqDetails: DetailedLogging[ThingRequest], srDetails: DetailedLogging[ServiceResponse]):
    Either[Fail, ThingResponse] = requestAndServiceResponse match {
      case RequestAndServiceResponse(req, res) if res.status.code == 200 => Right(ThingResponse(req.id, res.body.s))
      case reqAndSr => Left(failer.responseParserfailer(reqAndSr, "failed"))

    }
  }
}

abstract class MicroserviceBuilderSpec[M[_], Fail](monadName: String)(implicit val async: Async[M], val monad: MonadCanFailWithException[M, Fail]) extends UtilsSpec with MicroserviceComposers[M] with FunctionFixture {

  def builder = new MicroserviceBuilderForTest[M, Fail]
  def fail: Fail
  val exception = new RuntimeException("the error")

  def getFailure[X](block: => M[X]): Fail
  val serviceRequest = ServiceRequest(Method("get"), Uri("/thingyId"))

  val serviceResponse = ServiceResponse(Status(200), Body("thingyValue"), ContentType("who cares"))
  val serviceResponse202 = ServiceResponse(Status(202), Body("thingyValue"), ContentType("who cares"))

  behavior of s"MicroserviceBuilder for $monadName"

  it should "have an http(ServiceName) method that calls the http factory" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val serviceName = ServiceName("name")
    when(b.httpFactory.apply(serviceName)) thenReturn service

    b.http(serviceName) shouldBe service
  }

  it should "have an objectify that returns a M[Result] - happy path" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]

    val obj = service |+| b.objectify[ThingRequest, ThingResponse]
    when(service.apply(serviceRequest)) thenReturn serviceResponse.liftM
    obj(ThingRequest("thingyId")).await() shouldBe ThingResponse("thingyId", "thingyValue")

  }
  it should "have an objectify that returns a M[Result] - failure path" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val request = ThingRequest("thingyId")

    val obj = service |+| b.objectify[ThingRequest, ThingResponse]
    when(service.apply(serviceRequest)) thenReturn serviceResponse202.liftM
    when(b.failer.responseParserfailer(RequestAndServiceResponse(request, serviceResponse202), "failed")) thenReturn fail
    getFailure[ThingResponse](obj(request)) shouldBe fail

  }

  it should "have a metrics that records the metrics - success path" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val metricsData = Map("result" -> CountMetricValue)
    implicit val rd: ReportData[ServiceResponse] = new ReportData[ServiceResponse] {
      override def apply[Fail](prefix: String, duration: Long): Try[Either[Fail, ServiceResponse]] => Map[String, MetricValue] = {
        fn(Success(Right(serviceResponse)), metricsData)
      }
    }
    when(service.apply(serviceRequest)) thenReturn serviceResponse.liftM
    val mService: ServiceRequest => M[ServiceResponse] = service |+| b.metrics("somePrefix")


    mService(serviceRequest).await() shouldBe serviceResponse

    verify(b.putMetrics, times(1)).apply(metricsData)
  }
  it should "have a metrics that records the metrics - exception path" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val metricsData = Map("result" -> CountMetricValue)
    implicit val rd: ReportData[ServiceResponse] = new ReportData[ServiceResponse] {
      override def apply[Fail](prefix: String, duration: Long): Try[Either[Fail, ServiceResponse]] => Map[String, MetricValue] = {
        fn(Failure(exception), metricsData)
      }
    }
    when(service.apply(serviceRequest)) thenReturn exception.liftException[M, ServiceResponse]
    val mService: ServiceRequest => M[ServiceResponse] = service |+| b.metrics("somePrefix")


    intercept[RuntimeException](mService(serviceRequest).await()) shouldBe exception

    verify(b.putMetrics, times(1)).apply(metricsData)
  }


  it should "have a logging that wraps the delegate in a logging service" ignore {
    val pattern = "pattern {0} {2}"
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val lService = (service |+| b.logging[ServiceRequest, ServiceResponse](pattern)).asInstanceOf[LoggingService[M, Fail, ServiceRequest, ServiceResponse]]
    lService.delegate shouldBe service
    lService.messagePrefix shouldBe pattern
    lService.logReqAndResult shouldBe builder.logReqAndResult
  }

  it should "have a retry that wraps the service in a RetryService" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val retryConfig = RetryConfig(1, RandomDelay(Duration.fromNanos(1000)))

    val rService = (service |+| b.retry[ServiceRequest, ServiceResponse](retryConfig)).asInstanceOf[RetryService[M, Fail, ServiceRequest, ServiceResponse]]
    rService.delegate shouldBe service
    rService.retryConfig shouldBe retryConfig
  }

  it should "have a profile that wraps the service in a profile service" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val profileData = mock[TryProfileData]
    val pService = (service |+| b.profile[ServiceRequest, ServiceResponse](profileData)).asInstanceOf[ProfileService[M, ServiceRequest, ServiceResponse]]
    pService.delegate shouldBe service
    pService.profileData shouldBe profileData
  }

  it should "have a cache method that wraps the service with the result from the cache factory" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val profileData = mock[TryProfileData]
    val cache = mock[Cache[M, ServiceRequest, ServiceResponse]]

    implicit val s: ShouldCacheResult[ServiceResponse] = _ => true
    implicit val ck: CachableKey[ServiceRequest] = CachableKey.cachableKeyDefault[ServiceRequest]

    when(b.cacheFactory.apply("cacheName", service)) thenReturn cache
    service |+| b.cache[ServiceRequest, ServiceResponse]("cacheName") shouldBe cache
  }


}

class MicroserviceBuilderIdentityMonadSpec extends MicroserviceBuilderSpec[IdentityMonad, Throwable]("IdentityMonad") {
  val fail = exception
  override def getFailure[X](block: => IdentityMonad[X]): Throwable = try {
    block.await()
    fail()
  } catch {case e: Exception => e}
}
