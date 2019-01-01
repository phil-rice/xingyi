package one.xingyi.scriptSharedBackend.domain

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad
import one.xingyi.core.script._
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class CodeRequest()

object CodeRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, CodeRequest] = { sr => CodeRequest().liftM[M] }
}

case class Code[SharedE, DomainE](domainList: DomainList[SharedE, DomainE])

object Code extends JsonWriterLanguage {
  implicit def toServiceResponse[J, SharedE, DomainE](implicit jsonWriter: JsonWriter[J]): ToServiceResponse[CodeRequest, Code[SharedE, DomainE]] = {
    codeRequest =>
      code: Code[SharedE, DomainE] =>
        val summary: JsonValue = JsonList(code.domainList.domains.map { details: DomainDetails[SharedE, DomainE] =>
          JsonObject(
            "name" -> details.name,
            "code" -> JsonObject(details.code.toList.map { d => d._1.toString -> JsonString(d._2.hash) }: _*))
        })
        ServiceResponse(Status(200), Body(jsonWriter(summary)), ContentType(DomainDefn.xingyiCodeSummaryMediaType))
  }

}

case class CodeDetailsRequest(hash: String)

object CodeDetailsRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, CodeDetailsRequest] = { sr => CodeDetailsRequest(Strings.lastSection("/")(sr.uri.path.path)).liftM[M] }
}
case class CodeDetailsResponse(code: String, mediaType: MediaType)


object CodeDetailsResponse {
  implicit def toServiceResponse[J](implicit jsonWriter: JsonWriter[J]): ToServiceResponse[CodeDetailsRequest, CodeDetailsResponse] = cdreq => cdres =>
    ServiceResponse(Status(200), Body(cdres.code), ContentType(cdres.mediaType.s))
}
