package org.validoc.utils.parser

import org.validoc.utils.http.ContentType
import org.validoc.utils.{ParserException, UtilsWithLoggingSpec}

class ParserResultSpec extends UtilsWithLoggingSpec {

  behavior of "ParserResult"

  val runtimeException = new RuntimeException
  val ct = ContentType("someContentType")
  val parser = mock[Parser[String]]
  val found = FoundResult(ct, parser)
  val notFound = NotFoundResult[Parser[String]](ct, Set(ct))
  val error = ErrorResult[Parser[String]](ct, runtimeException)

  it should "have a map function" in {
    found.map(_ => "mapped") shouldBe FoundResult(ct, "mapped")
    notFound.map(_ => "mapped") shouldBe NotFoundResult[String](ct, Set(ct))
    error.map(_ => "mapped") shouldBe ErrorResult[String](ct, runtimeException)
  }

  it should "have a valueOrDefault method" in {
    val defaultResult = mock[Parser[String]]
    found.valueOrDefault(defaultResult) shouldBe parser
    notFound.valueOrDefault(defaultResult) shouldBe defaultResult
    intercept[ParserException](error.valueOrException).parserResult shouldBe error
  }
  it should "have a valueOrException method" in {
    found.valueOrException shouldBe parser
    intercept[ParserException](notFound.valueOrException).parserResult shouldBe notFound
    intercept[ParserException](error.valueOrException).parserResult shouldBe error
  }

}
