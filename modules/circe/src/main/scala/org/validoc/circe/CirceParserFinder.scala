package org.validoc.circe

import io.circe.{DecodingFailure, _}
import io.circe.syntax._
import org.validoc.utils.either.Eithers._
import org.validoc.utils.parser.ParserFinder

import scala.reflect.ClassTag
case class MalformedJson(entity: String, json: String)(decodingFailure: DecodingFailure) extends RuntimeException(s"Entity: $entity Failure was $decodingFailure\nJson was\n$json")

object CirceParserFinder {

  def allways[T: ClassTag: Decoder]: ParserFinder[T] = {
    val entityName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    ParserFinder.always(s => s.asJson.as[T].getOrException(MalformedJson(entityName, s)))
  }
}
