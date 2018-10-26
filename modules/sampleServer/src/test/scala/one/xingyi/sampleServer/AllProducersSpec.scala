package one.xingyi.sampleServer
import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.{CacheFactory, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.http._
import one.xingyi.core.logging.{LogRequestAndResult, LoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PutMetrics
import one.xingyi.core.monad.IdentityMonad
import org.json4s.JsonAST.JValue
import org.scalatest.BeforeAndAfterAll
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.core.monad.AsyncForScalaFuture._

import scala.concurrent.{Await, Future}
import scala.util.Success

class AllProducersSpec extends UtilsSpec with AllProducersSetup {
  //implicit val cacheFactory = mock[CachingServiceFactory[Future]]
  val serviceRequest = ServiceRequest(Get, Uri("/someUri"), Seq(), Some(Body("theBody")))
  val serviceResponse = ServiceResponse(Status(200), Body("response: theBody"), List(ContentType("text/html")))

  behavior of "AllProducers"

  it should "have a smoke test that just creates it and checks it doesn't throw an exception" in {
    val setup = new AllProducers[Future, JValue, Throwable](1000)
    capturePrintln(setup.logReqAndResult.apply[ServiceRequest, ServiceResponse](this, "somePrefix") apply(serviceRequest, Success(Right(serviceResponse))))._2.trim shouldBe
    "[DEBUG] success.success:ServiceRequest(Get,None,Path(/someUri),WrappedArray(),List(),Some(Body(theBody))),ServiceRequest(Get,None,Path(/someUri),WrappedArray(),List(),Some(Body(theBody))),ServiceResponse(Status(200),Body(response: theBody),List(ContentType(text/html))),ServiceResponse(Status(200),Body(response: theBody),List(ContentType(text/html)))"

    await(setup.httpFactory.apply(ServiceName("unused"))(serviceRequest)) shouldBe serviceResponse

  }

}
