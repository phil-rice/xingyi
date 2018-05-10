package one.xingyi.core.cache

import scala.util.Try

trait Id

case class StringId(id: String) extends Id

case class IntId(id: String) extends Id

case class ObjectId[T](t: T) extends Id

case object UnitId extends Id

