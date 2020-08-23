/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.parser

import one.xingyi.core.http.ContentType
import one.xingyi.core.json.{FromJson, FromJsonLib, JsonParser}
import one.xingyi.core.monad.{Functor, MonadCanFail}

import scala.annotation.implicitNotFound
import scala.language.higherKinds
trait  Parser[T] extends (String => T)

object Parser {
  implicit def default[J, T](implicit jsonParser: JsonParser[J], fromJsonLib: FromJsonLib[J, T]): FromJson[T] = s => fromJsonLib(jsonParser(s))
}

trait ParserFailer[Fail] {
  def cannotFindParser(contentType: Option[ContentType]): Fail
}

@implicitNotFound("ParserFinder of type [${T}] not found. Is it because you need another implicit in scope? The simplest way to get one in scope if there is only one JSON representation is to implement FromJson[T]")
trait ParserFinder[M[_], T] {
  protected def m: Functor[M]
  def find(contentType: Option[ContentType]): M[Parser[T]]
  def apply(v1: Option[ContentType], v2: String): M[T] = m.map[Parser[T], T](find(v1), parser => parser(v2))
}

case class AlwaysParserFinder[M[_], T](parser: Parser[T])(implicit val m: Functor[M]) extends ParserFinder[M, T] {
  override def find(contentType: Option[ContentType]): M[Parser[T]] = m.liftM(parser)
}

case class MapParserFinder[M[_], Fail, T](map: Map[Option[ContentType], Parser[T]])(implicit val m: MonadCanFail[M, Fail], parserFailer: ParserFailer[Fail]) extends ParserFinder[M, T] {
  override def find(contentType: Option[ContentType]) = map.get(contentType) match {
    case Some(parser) => m.liftM(parser)
    case None => m.fail(parserFailer.cannotFindParser(contentType))
  }

}

object ParserFinder {
  implicit def alwaysSameParser[M[_] : Functor, T](implicit fromJson: FromJson[T]): ParserFinder[M, T] = always[M, T](fromJson)
  def always[M[_] : Functor, T](parser: Parser[T]) = AlwaysParserFinder[M, T](parser)
  def fromMap[M[_], Fail, T](map: Map[Option[ContentType], Parser[T]])(implicit monadCanFail: MonadCanFail[M, Fail], parserFailer: ParserFailer[Fail]) = MapParserFinder[M, Fail, T](map)
}
