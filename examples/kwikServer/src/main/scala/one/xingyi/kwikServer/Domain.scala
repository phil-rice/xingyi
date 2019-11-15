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
import scala.io.Source
import scala.language.higherKinds

case class PomBundle(envVariables: Seq[Variable], systemProperties: Seq[Variable], pomData: Seq[PomData])

object PomBundle {
  def parse(s: Iterator[String]) =
    PomBundle(
      Variable.parseList("environment")(s),
      Variable.parseList("system")(s),
      TitleLengthValue.parseList(s).map(PomData.apply))

  implicit def ServiceRequestToPomBundle[M[_] : Liftable]: FromServiceRequest[M, PomBundle] =
    sr => PomBundle.parse(Source.fromString(sr.body.getOrElse(throw new RuntimeException("expected a body and it was empty")).s).getLines()).liftM[M]
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


