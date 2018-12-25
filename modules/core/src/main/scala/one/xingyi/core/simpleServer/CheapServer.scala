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


abstract class CheapServer[M[_], Fail](port: Int)(implicit val async: Async[M],
                                       val failer: Failer[Fail],
                                       val timeService: NanoTimeService,
                                       val detailedLoggingForSR: DetailedLogging[ServiceResponse],
                                       implicit val loggingAdapter: LoggingAdapter)
  extends MicroserviceBuilder[M, Fail] with MicroserviceComposers[M] with EnrichLanguage[M]
    with MergeLanguage[M] with AnyLanguage with MonadLanguage with AsyncLanguage {
  def endpoints: ServiceRequest => M[Option[ServiceResponse]]

  implicit def httpFactory = new HttpFactory[M, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req =>
      monad.liftM(ServiceResponse(Status(200), Body(s"response: ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html")))
    }
  }
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val cacheFactory = new CachingServiceFactory[M](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)

  implicit val executors = Executors.newFixedThreadPool(10)

  def start = {
    val server = new SimpleHttpServer(port, new EndpointHandler[M, Fail](endpoints))
    server.start()
    server
  }

}