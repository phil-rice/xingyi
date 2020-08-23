/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.http.FromServiceRequest
import one.xingyi.core.json._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.{Monad, MonadCanFailWithException}

import scala.language.higherKinds

case class EntityServiceFinderRequest(host: String)
object EntityServiceFinderRequest {
  implicit def fromServiceRequest[M[_], Fail](implicit monad: MonadCanFailWithException[M, Fail], failer: NoHostFailer[Fail]): FromServiceRequest[M, EntityServiceFinderRequest] =
    sr => failer.failOrUseHost(sr)(host => EntityServiceFinderRequest(host).liftM)
}

case class EntityServiceFinderResponse(hostAndPort: String, codePattern: String, urlPattern: String, supportedVerbs: List[String], domainList: DomainList[_, _])
object EntityServiceFinderResponse extends JsonWriterLanguage {
  implicit def toJson[J: JsonWriter]: ToJsonLib[EntityServiceFinderResponse] = { res =>
    import res._
    def toPath(hash: String) = codePattern.replace("<host>", hostAndPort).replace("<hash>", hash)

    JsonObject(
      "url" -> urlPattern.replace("<host>", hostAndPort), "verbs" -> JsonList(supportedVerbs.map(JsonString.apply)),
      "domains" -> JsonList(
        domainList.domains.map { d =>
          JsonObject(
            "domain" -> d.name,
            "render" -> JsonList(d.renderers.sorted.map(JsonString.apply)),
            "lens" -> JsonList(d.lensNames.toList.sorted.map(JsonString.apply))
          )

        }
      ))
  }
}
