package one.xingyi.core.script

import one.xingyi.core.http.FromServiceRequest
import one.xingyi.core.json._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Monad

import scala.language.higherKinds

case class EntityServiceFinderRequest(host: String)
object EntityServiceFinderRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, EntityServiceFinderRequest] = sr => EntityServiceFinderRequest(sr.host).liftM[M]
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
