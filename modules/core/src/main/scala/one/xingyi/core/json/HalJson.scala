package one.xingyi.core.json

import one.xingyi.core.http.{Get, HasMethod, Method}

object HalJson extends HalJson
trait HalJson extends JsonWriterLanguage {
  def link(url: String, method: Method): JsonObject =
    JsonObject("href" -> url).
      addIf(method != Get, "method", method.toString).
      addIf(url.contains("{"), "templated", true)
}
