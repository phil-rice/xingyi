package one.xingyi.core.simpleServer

import java.util.ResourceBundle
import java.util.concurrent.{Executors, TimeUnit}

import one.xingyi.core.aggregate.{EnrichLanguage, MergeLanguage}
import one.xingyi.core.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.endpoint.ChainKleisli
import one.xingyi.core.http._
import one.xingyi.core.language._
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult, LoggingAdapter, PrintlnLoggingAdapter}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PrintlnPutMetrics
import one.xingyi.core.monad.{Async, IdentityMonad, MonadCanFailWithException}
import one.xingyi.core.time.NanoTimeService

import scala.language.higherKinds


object CheapServer{
}
class CheapServer[M[_] : Async, Fail](port: Int, endpoints: (ServiceRequest => M[Option[ServiceResponse]])*)
                                     (implicit val monad: MonadCanFailWithException[M, Fail], val failer: Failer[Fail]) extends ChainKleisli[M, Fail] {


  implicit val executors = Executors.newFixedThreadPool(10)


  def start: SimpleHttpServer = {
    val server = new SimpleHttpServer(port, new EndpointHandler[M, Fail](chain(endpoints: _*)))
    server.start()
    server
  }
   def stop(server: SimpleHttpServer) = {
     server.stop()
     executors.shutdownNow()
     executors.awaitTermination(1000, TimeUnit.MILLISECONDS)
   }

}