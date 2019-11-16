package one.xingyi.kwikServer

import one.xingyi.core.cache.{CachableKey, Id, ShouldCacheResult, StringId}
import one.xingyi.core.crypto.Digestor
import one.xingyi.core.http._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad._

import scala.io.Source
import scala.language.higherKinds
import scala.util.Try

case class PomBundle(repositoryUrls: Seq[String], envVariables: Seq[Variable], systemProperties: Seq[Variable], pomData: Seq[PomData], hash: String)

object PomBundle {
  def parse(s: String)(implicit digestor: Digestor) = {
    val iterator = Source.fromString(s).getLines()
    PomBundle(
      TitleLengthValue.parseAndValidate("repositories")(iterator).value,
      Variable.parseList("environment")(iterator),
      Variable.parseList("system")(iterator),
      TitleLengthValue.parseList(iterator).map(PomData.apply),
      digestor(s))
  }

  implicit object cachingKeyForPomBundle extends CachableKey[PomBundle] {
    override def id(req: PomBundle): Id = StringId(req.hash)

    override def bypassCache(req: PomBundle): Boolean = false
  }

  implicit def ServiceRequestToPomBundle[M[_] : Liftable]: FromServiceRequest[M, PomBundle] =
    sr => PomBundle.parse(sr.body.getOrElse(throw new RuntimeException("expected a body and it was empty")).s).liftM[M]
}

case class PomData(relativePath: String, pom: String)

object PomData {
  def apply(tlv: TitleLengthValue): PomData = PomData(tlv.title, tlv.value.mkString("\n"))
}


case class KwikResult(jars: List[String], poms: List[String], debugHash: String)

object KwikResult {
  implicit def kwikResultToServiceResponse[M[_] : Liftable]: ToServiceResponse[PomBundle, KwikResult] =
    pomBundle => kwikResult => ServiceResponse(Status(200), Body(pomBundle.toString + "\n\n" + kwikResult.toString), ContentType("text/plain"))

  implicit object shouldCacheResultForKwikResult extends ShouldCacheResult[KwikResult] {
    override def shouldCacheStrategy(req: Try[KwikResult]): Boolean = true
  }

}


