package one.xingyi.kwikServer

import java.util.concurrent.{Executor, Executors}

import one.xingyi.core.endpoint.{EndpointKleisli, MatchesServiceRequest}
import one.xingyi.core.http._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.local.ExecutionContextWithLocal
import one.xingyi.core.logging.{DetailedLogging, PrintlnLoggingAdapter}
import one.xingyi.core.monad._
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.time.NanoTimeService

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

case class PomBundle(envVariables: Seq[Variable], systemProperties: Seq[Variable], pomData: Seq[PomData])

object PomBundle {
  def parse(s: Iterator[String]) =
    PomBundle(
      Variable.parseList("environment")(s),
      Variable.parseList("system")(s),
      TitleLengthValue.parseList(s).map(PomData.apply))

  implicit def ServiceRequestToPomBundle[M[_] : Liftable]: FromServiceRequest[M, PomBundle] =
    sr => PomBundle(List(), List(Variable("hello", "world")), List()).liftM[M]
}

case class PomData(relativePath: String, pom: String)

object PomData {
  def apply(tlv: TitleLengthValue): PomData = PomData(tlv.title, tlv.value.mkString("\n"))
}


case class KwikResult(jars: List[String], poms: List[String], debugHash: String)

object KwikResult {
  implicit def kwikResultToServiceResponse[M[_] : Liftable]: ToServiceResponse[PomBundle, KwikResult] =
    pomBundle => kwikResult => ServiceResponse(Status(200), Body(pomBundle.toString + "\n\n" + kwikResult.toString), ContentType("text/plain"))
}


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


  val endpoint: ServiceRequest => M[Option[ServiceResponse]] = function[PomBundle, KwikResult]("findDependanciesFromPom")(query => KwikResult(List(), List(), "debugHash")) |+| endpoint[PomBundle, KwikResult]("/deps", MatchesServiceRequest.fixedPath(Get))

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

