/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class CodeRequest()

object CodeRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, CodeRequest] = { sr => CodeRequest().liftM[M] }
}

case class CodeResponse[SharedE, DomainE](domainList: DomainList[SharedE, DomainE])

object CodeResponse extends JsonWriterLanguage {
  implicit def toServiceResponse[J, SharedE, DomainE](implicit jsonWriter: JsonWriter[J]): ToServiceResponse[CodeRequest, CodeResponse[SharedE, DomainE]] = {
    codeRequest =>
      code: CodeResponse[SharedE, DomainE] =>
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

