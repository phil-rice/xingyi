package one.xingyi.core.simpleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import one.xingyi.core.aggregate.{EnrichLanguage, MergeLanguage}
import one.xingyi.core.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.http._
import one.xingyi.core.language._
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult, LoggingAdapter, PrintlnLoggingAdapter}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PrintlnPutMetrics
import one.xingyi.core.monad.{Async, IdentityMonad, MonadCanFailWithException}
import one.xingyi.core.time.NanoTimeService

import scala.language.higherKinds


class CheapServer[M[_] : Async, Fail: Failer](port: Int, endpoints: ServiceRequest => M[Option[ServiceResponse]])
                                             (implicit monad: MonadCanFailWithException[M, Fail]) {

  implicit val executors = Executors.newFixedThreadPool(10)

  def start = {
    val server = new SimpleHttpServer(port, new EndpointHandler[M, Fail](endpoints))
    server.start()
    server
  }

}