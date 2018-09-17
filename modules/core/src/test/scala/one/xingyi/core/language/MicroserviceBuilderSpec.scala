package one.xingyi.core.language

import one.xingyi.core.{FunctionFixture, UtilsSpec}
import one.xingyi.core.cache.CacheFactory
import one.xingyi.core.http._
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult}
import one.xingyi.core.metrics.{CountMetricValue, MetricValue, PutMetrics, ReportData}
import one.xingyi.core.monad.{Async, IdentityMonad, Liftable, MonadCanFailWithException}
import one.xingyi.core.time.{MockTimeService, NanoTimeService}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import Language._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class MicroserviceBuilderForTest[M[_], Fail](implicit val async: Async[M], val monad: MonadCanFailWithException[M, Fail], val detailedLoggingForSR: DetailedLogging[ServiceResponse]) extends MicroserviceBuilder[M, Fail] with MockitoSugar {
  override val cacheFactory: CacheFactory[M] = mock[CacheFactory[M]]
  override implicit val timeService: NanoTimeService = new MockTimeService
  override val logReqAndResult: LogRequestAndResult[Fail] = mock[LogRequestAndResult[Fail]]
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

  it should "have a logging that logs the requests and responses" in {
    val b = builder
    val service = mock[ServiceRequest => M[ServiceResponse]]
    val pattern = "pattern {0} {1}"
    val lService: ServiceRequest => M[ServiceResponse] = service |+| b.logging(pattern)
    when(service.apply(serviceRequest)) thenReturn serviceResponse.liftM
    val log = mock[(ServiceRequest, Try[Either[Fail, ServiceResponse]]) => Unit]
    when(b.logReqAndResult.apply[ServiceRequest, ServiceResponse](pattern, pattern)).thenReturn(log)
    lService(serviceRequest).await shouldBe serviceResponse
    verify(log, times(1)).apply(serviceRequest, Success(Right(serviceResponse)))
  }


}

class MicroserviceBuilderIdentityMonadSpec extends MicroserviceBuilderSpec[IdentityMonad, Throwable]("IdentityMonad") {
  val fail = exception
  override def getFailure[X](block: => IdentityMonad[X]): Throwable = try {
    block.await()
    fail()
  } catch {case e: Exception => e}
}
