package org.validoc.utils.parser

import org.validoc.utils.{Parser, UtilsSpec}
import org.validoc.utils.http.ContentType

class ParserResultSpec extends UtilsSpec {

  behavior of "ParserResult"

  it should "have a map function" in {
    val ct = ContentType("someContentType")
    val parser = mock[Parser[String]]
    val found = FoundResult(ct, parser)
    val notFound = NotFoundResult[Parser[String]](ct, Set(ct))

    found.map(_ => "mapped") shouldBe FoundResult(ct, "mapped")
    notFound.map(_ => "mapped") shouldBe NotFoundResult[String](ct, Set(ct))
  }

}
