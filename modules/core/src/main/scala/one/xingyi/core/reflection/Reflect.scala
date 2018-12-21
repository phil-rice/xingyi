package one.xingyi.core.reflection

import java.lang.reflect.Field

import scala.reflect.ClassTag

object Reflect {
  def fields[T](implicit classTag: ClassTag[T]): List[Field] = classTag.runtimeClass.getFields.toList
  def typedFields[T: ClassTag, F](implicit classTag: ClassTag[F]): List[Field] = fields[T].filter(f => classTag.runtimeClass.isAssignableFrom(f.getType))
  def apply[T: ClassTag, F](t: T) = new Reflect(t)
}

class Reflect[T](t: T)(implicit classTag: ClassTag[T]) {
  def fieldValues[F:ClassTag]: List[F] = Reflect.typedFields[T, F].map(_.get(t).asInstanceOf[F])
  def fieldNameAndValues[F:ClassTag]: List[(String, F)] = Reflect.typedFields[T, F].map { f => (f.getName, f.get(t).asInstanceOf[F]) }

}