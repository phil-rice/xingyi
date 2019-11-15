package one.xingyi.kwikServer

import java.util.concurrent.{Executor, Executors}

import one.xingyi.core.endpoint.{EndpointKleisli, MatchesServiceRequest}
import one.xingyi.core.http.{Failer, Get, Post, ServiceRequest, ServiceResponse}
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.local.ExecutionContextWithLocal
import one.xingyi.core.logging.{DetailedLogging, PrintlnLoggingAdapter}
import one.xingyi.core.monad.{Async, IdentityMonad, LiftFunctionKleisli, MonadCanFailWithException}
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.time.NanoTimeService

import scala.concurrent.ExecutionContext


class KwikEndpoints[M[_], Fail](implicit executor: Executor,
                                val monad: MonadCanFailWithException[M, Fail],
                                val async: Async[M],
                                val failer: Failer[Fail],
                                val timeService: NanoTimeService,
                                val detailedLoggingForSR: DetailedLogging[ServiceResponse]) extends EndpointKleisli[M] with LiftFunctionKleisli[M] with MicroserviceComposers[M] {


  //pseudo code
  //1: Make a directory, copying the POMS into the correct places
  //2: Make a settings.xml points to a blank .m2 and my big .m2
  //3: get the dependancies


  val endpoint: ServiceRequest => M[Option[ServiceResponse]] = function[PomBundle, KwikResult]("findDependanciesFromPom")(query => KwikResult(List(), List(), "debugHash")) |+| endpoint[PomBundle, KwikResult]("/deps", MatchesServiceRequest.fixedPath(Post))

}


object KwikServer extends App {
  implicit val executors = Executors.newFixedThreadPool(10)
  implicit val exc = new ExecutionContextWithLocal(ExecutionContext.fromExecutor(executors))

  implicit val loggingAdapter = PrintlnLoggingAdapter

  import one.xingyi.core.http.Failer.failerForThrowable

  val kwikEndpoint = new KwikEndpoints[IdentityMonad, Throwable]()
  val server = new SimpleHttpServer(9000, new EndpointHandler[IdentityMonad, Throwable](kwikEndpoint.endpoint))

  server.start()
}
