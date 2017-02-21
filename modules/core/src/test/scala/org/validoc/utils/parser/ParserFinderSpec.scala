package org.validoc.utils.parser

import org.mockito.Mockito._
import org.validoc.utils.http.ContentType
import org.validoc.utils.{Parser, UtilsSpec}

abstract class AbstractParserSpec[T <: ParserFinder[String]] extends UtilsSpec {


  val ctMain = ContentType("main")
  val ct1 = ContentType("one")
  val ct2 = ContentType("two")

  def makeParserFinderForCtMain(parser: Parser[String]): ParserFinder[String]

  def setup(fn: (ParserFinder[String], Parser[String]) => Unit): Unit = {
    val parser = mock[Parser[String]]

    fn(makeParserFinderForCtMain(parser), parser)
  }

  behavior of getClass.getSimpleName

  it should "return FoundResult(ctMain,parser) when ctMain is the contentType " in {
    setup { (finder, parser) =>
      finder.find(ctMain) shouldBe FoundResult(ctMain, parser)
    }
  }

  it should "have a apply method that finds a parser and then applies it" in {
    setup { (finder, parser) =>
      when(parser.apply("someString")) thenReturn ("someResult")
      finder(ctMain, "someString") shouldBe FoundResult(ctMain, "someResult")
    }

  }
}

class AlwaysParserFinderTest extends AbstractParserSpec[AlwaysParserFinder[String]] {


  it should "always find the same parser" in {
    setup { (finder, parser) =>
      finder.find(ct1) shouldBe FoundResult(ct1, parser)
      finder.find(ct2) shouldBe FoundResult(ct2, parser)
    }
  }

  override def makeParserFinderForCtMain(parser: Parser[String]): ParserFinder[String] = ParserFinder.always(parser)
}

class FromMapParserFinderTest extends AbstractParserSpec[MapParserFinder[String]] {

  val parser1 = mock[Parser[String]]

  override def makeParserFinderForCtMain(parser: Parser[String]): ParserFinder[String] =
    ParserFinder.fromMap(Map(ctMain -> parser, ct1 -> parser1))


  it should "find the registered parser for the content type" in {
    setup { (finder, parser) =>
      finder.find(ct1) shouldBe FoundResult(ct1, parser1)
    }
  }

  it should "not find parsers for unregistered content types" in {
    setup { (finder, parser) =>
      finder.find(ct2) shouldBe NotFoundResult(ct2, Set(ctMain, ct1))
    }
  }
}