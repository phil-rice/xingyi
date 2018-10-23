package one.xingyi.sampleServer
import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.DurationStaleCacheStategy
import one.xingyi.core.http._
import one.xingyi.core.logging.{LogRequestAndResult, LoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PutMetrics
import org.scalatest.BeforeAndAfterAll

//This is an experimental class so only simple and smoke tests
class AllProducersSpec extends UtilsSpec with BeforeAndAfterAll{
//
//  behavior of "AllProducers"
//
//  val server = new AllProducers(10001)
//  override protected def afterAll(): Unit = {
//    server.server.stop()
//    super.afterAll()
//  }
//  import server._
//  it should "have an httpfactory that is faked up" in {
//    await(httpFactory(ServiceName("someServiceName")) apply ServiceRequest(Method("get"), Uri("/someUri"), body = Some(Body("someBody")))) shouldBe ServiceResponse(Status(200), Body("response: someBody"), ContentType("text/html"))
//  }
//
//  it should "have a println logging adapter" in {
//    implicitly[LoggingAdapter] shouldBe server.loggingAdapter
//  }
//
//  it should "have a resource bundle using 'messages' " in {
//    resourceBundle.getString("some.message") shouldBe "the message"
//  }
//
//  it should "have a println put metrics" in {
//    implicitly[PutMetrics] shouldBe server.putMetrics
//  }
//  it should "have a SimpleLogRequestAndResult " in {
//    implicitly[LogRequestAndResult[Throwable]] shouldBe logRequestAndResult
//    logRequestAndResult.asInstanceOf[SimpleLogRequestAndResult].loggingAdapter shouldBe server.loggingAdapter
//  }
//  it should "have a caching service factory" in {
//    cacheFactory.sizeStrategy shouldBe NoMapSizeStrategy
//    cacheFactory.cachingStrategy shouldBe DurationStaleCacheStategy(10000000000L, 10000000000000L)
//  }


}
