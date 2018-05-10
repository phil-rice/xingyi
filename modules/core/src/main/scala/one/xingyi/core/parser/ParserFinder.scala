package one.xingyi.core.parser

import one.xingyi.core.functions.{Functor, MonadCanFail}
import one.xingyi.core.http.ContentType
import one.xingyi.core.json.FromJson

import scala.annotation.implicitNotFound
import scala.language.higherKinds

trait Parser[T] extends (String => T)

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
  implicit def alwaysSameParser[M[_]:Functor, T](implicit fromJson: FromJson[T]):ParserFinder[M, T] = always[M, T](fromJson)
  def always[M[_]: Functor,T](parser: Parser[T]) = AlwaysParserFinder[M, T](parser)
  def fromMap[M[_], Fail, T](map: Map[Option[ContentType], Parser[T]]) (implicit monadCanFail: MonadCanFail[M, Fail],parserFailer: ParserFailer[Fail])= MapParserFinder[M, Fail, T](map)
}