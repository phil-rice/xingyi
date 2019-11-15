package one.xingyi.kwikServer

import java.util.concurrent.{Executor, Executors}

import one.xingyi.core.cache.{CacheFactory, CacheKleisli, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.endpoint.{ChainKleisli, EndpointKleisli, MatchesServiceRequest}
import one.xingyi.core.http.{Body, ContentType, Failer, Get, Post, ServiceRequest, ServiceResponse, Status, ToServiceResponse}
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.local.ExecutionContextWithLocal
import one.xingyi.core.logging.{DetailedLogging, PrintlnLoggingAdapter}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.monad.{Async, IdentityMonad, LiftFunctionKleisli, MonadCanFailWithException}
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.time.NanoTimeService
import one.xingyi.core.language.AnyLanguage._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

case class CacheResult[M[_]](cacheFactory: CacheFactory[M])

object CacheResult {
  implicit def cacheResultToServiceResponse[M[_]]: ToServiceResponse[ServiceRequest, CacheResult[M]] = {
    sr =>
      cr =>
        ServiceResponse(
          Status(200),
          Body(cr.cacheFactory.entries.map { case (name, cache) => name + "\n" + cache.cachingMetrics.toString }.mkString("\n\n")),
          ContentType("text/plain"))
  }

}

class KwikEndpoints[M[_], Fail](implicit executor: Executor,
                                val monad: MonadCanFailWithException[M, Fail],
                                val async: Async[M],
                                val failer: Failer[Fail],
                                val cacheFactory: CacheFactory[M],
                                val timeService: NanoTimeService,
                                val detailedLoggingForSR: DetailedLogging[ServiceResponse]) extends
  EndpointKleisli[M] with LiftFunctionKleisli[M] with MicroserviceComposers[M] with CacheKleisli[M] with ChainKleisli[M,Fail] {


  //pseudo code
  //1: Make a directory, copying the POMS into the correct places
  //2: Make a settings.xml points to a blank .m2 and my big .m2
  //3: get the dependancies

  private val bizlogic: PomBundle => KwikResult = { query => println(s"query was $query"); KwikResult(List(), List(), query.hash) }
  val depsEndpoint: ServiceRequest => M[Option[ServiceResponse]] =
    function[PomBundle, KwikResult]("findDependanciesFromPom")(bizlogic) |+|
      cache[PomBundle, KwikResult]("theonlycache") |+|
      endpoint[PomBundle, KwikResult]("/deps", MatchesServiceRequest.fixedPath(Post))
  val cachingStatusEndpoint: ServiceRequest => M[Option[ServiceResponse]] =
    function[ServiceRequest, CacheResult[M]]("status")(sr => CacheResult(cacheFactory)) |+|
      endpoint[ServiceRequest, CacheResult[M]]("/cache", MatchesServiceRequest.fixedPath(Get))
  val microservice=chain(depsEndpoint, cachingStatusEndpoint)


}


object KwikServer extends App {
  implicit val executors = Executors.newFixedThreadPool(10)
  implicit val exc = new ExecutionContextWithLocal(ExecutionContext.fromExecutor(executors))

  implicit val loggingAdapter = PrintlnLoggingAdapter

  import one.xingyi.core.http.Failer.failerForThrowable

  private val min = 60l * 1000l * 1000l * 1000l
  private val tenMin = 10l * min
  private val tenHours = 10l * 60 * min

  private val staleCacheStrategy = DurationStaleCacheStategy(tenMin, tenHours)
  implicit val cachingService = new CachingServiceFactory[IdentityMonad](staleCacheStrategy, NoMapSizeStrategy)

  val kwikEndpoint = new KwikEndpoints[IdentityMonad, Throwable]()
  val server = new SimpleHttpServer(9000, new EndpointHandler[IdentityMonad, Throwable](kwikEndpoint.microservice))

  server.start()
}
