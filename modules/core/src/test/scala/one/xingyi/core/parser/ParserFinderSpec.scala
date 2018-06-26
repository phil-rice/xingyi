/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.parser

import one.xingyi.core.http.ContentType
import one.xingyi.core.json.FromJson
import one.xingyi.core.monad.{ContainerSpec, Functor, MonadCanFail}
import org.mockito.Mockito._

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

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class AlwaysParserFinderForScalaFuture extends AlwaysParserFinderTest[Future] {
  override def liftA[T](t: T) = Future.successful(t)
  override def getT[X](a: Future[X]) = Await.result(a, 5 seconds)

}
class FromMapParserFinderTestForFuture extends FromMapParserFinderTest[Future, Throwable] {
  override def liftA[T](t: T) = Future.successful(t)
  override def getT[X](a: Future[X]) = Await.result(a, 5 seconds)
  override def getFailure[T](m: Future[T]) = intercept[Throwable](getT(m))
}
