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
import scala.concurrent.Future

class AllProducersSpec extends UtilsSpec with AllProducersSetup {
  //implicit val cacheFactory = mock[CachingServiceFactory[Future]]

  behavior of "AllProducers"

  it should "have a smoke test that just creates it and checks it doesn't throw an exception" in {
    new AllProducers[Future, JValue, Throwable](1000)
  }

}
