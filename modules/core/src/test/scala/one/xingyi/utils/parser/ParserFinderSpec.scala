package one.xingyi.utils.parser

import org.mockito.Mockito._
import one.xingyi.utils.functions.{ContainerSpec, Functor, MonadCanFail}
import one.xingyi.utils.http.ContentType
import one.xingyi.utils.json.FromJson

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag

abstract class AbstractParserFinderSpec[M[_] : Functor, T <: ParserFinder[M, String]] extends ContainerSpec[M] {


  val ctMain = ContentType("main")
  val ct1 = ContentType("one")
  val ct2 = ContentType("two")

  def makeParserFinderForCtMain(parser: Parser[String]): ParserFinder[M, String]

  def setup(fn: (ParserFinder[M, String], Parser[String]) => Unit): Unit = {
    val parser = mock[Parser[String]]

    fn(makeParserFinderForCtMain(parser), parser)
  }

  behavior of getClass.getSimpleName

  it should "return M[parser] when ctMain is the contentType " in {
    setup { (finder, parser) =>
      getT(finder.find(Some(ctMain))) shouldBe parser
    }
  }

  it should "have a apply method that finds a parser and then applies it" in {
    setup { (finder, parser) =>
      when(parser.apply("someString")) thenReturn ("someResult")
      getT(finder(Some(ctMain), "someString")) shouldBe "someResult"
    }
  }

}

abstract class AlwaysParserFinderTest[M[_] : Functor] extends AbstractParserFinderSpec[M, AlwaysParserFinder[M, String]] {


  it should "always find the same parser" in {
    setup { (finder, parser) =>
      getT(finder.find(Some(ct1))) shouldBe parser
      getT(finder.find(Some(ct2))) shouldBe parser
      getT(finder.find(None)) shouldBe parser
    }
  }

  it should "be the default" in {
    implicit object FromJsonForString extends FromJson[String] {
      override def apply(v1: String) = v1 + " someString"
    }
    getT(implicitly[ParserFinder[M, String]].apply(None, "someInput")) shouldBe "someInput someString"
  }


  override def makeParserFinderForCtMain(parser: Parser[String]): ParserFinder[M, String] = ParserFinder.always(parser)
}


abstract class FromMapParserFinderTest[M[_], Fail <: AnyRef : ClassTag](implicit monadCanFail: MonadCanFail[M, Fail]) extends AbstractParserFinderSpec[M, MapParserFinder[M, Fail, String]] {

  def getFailure[T](m: M[T]): Fail

  val parser1 = mock[Parser[String]]
  val fail = mock[Fail]

  override def makeParserFinderForCtMain(parser: Parser[String]): ParserFinder[M, String] = {
    implicit object parserFailer extends ParserFailer[Fail] {
      override def cannotFindParser(contentType: Option[ContentType]) = fail
    }
    ParserFinder.fromMap(Map(Some(ctMain) -> parser, Some(ct1) -> parser1))
  }


  it should "find the registered parser for the content type" in {
    setup { (finder, parser) =>
      getT(finder.find(Some(ct1))) shouldBe parser1
    }
  }

  it should "not find parsers for unregistered content types" in {
    setup { (finder, parser) =>
      getFailure(finder.find(Some(ct2))) shouldBe fail
      getFailure(finder.find(None)) shouldBe fail
    }
  }

}

import one.xingyi.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.utils.functions.AsyncForScalaFuture._

class AlwaysParserFinderForScalaFuture extends AlwaysParserFinderTest[Future] {
  override def liftA[T](t: T) = Future.successful(t)
  override def getT[X](a: Future[X]) = Await.result(a, 5 seconds)

}
class FromMapParserFinderTestForFuture extends FromMapParserFinderTest[Future, Throwable] {
  override def liftA[T](t: T) = Future.successful(t)
  override def getT[X](a: Future[X]) = Await.result(a, 5 seconds)
  override def getFailure[T](m: Future[T]) = intercept[Throwable](getT(m))
}