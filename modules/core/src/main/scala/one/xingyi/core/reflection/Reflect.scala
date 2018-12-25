package one.xingyi.core.reflection

import java.lang.reflect.{Field, Method}

import scala.reflect.ClassTag
case class ReflectException(msg: String) extends RuntimeException
object Reflect {

  def fields[T](implicit classTag: ClassTag[T]): List[Field] = classTag.runtimeClass.getFields.toList
  def typedFields[T: ClassTag, F](implicit classTag: ClassTag[F]): List[Field] = fields[T].filter(f => classTag.runtimeClass.isAssignableFrom(f.getType))

  def methods[T](implicit classTag: ClassTag[T]): List[Method] = classTag.runtimeClass.getMethods.toList
  def typedMethods[T: ClassTag, F](implicit classTag: ClassTag[F]): List[Method] = methods[T].filter(method => classTag.runtimeClass.isAssignableFrom(method.getReturnType))
  def findParentClassWith[T](clazz: Class[_], target: Class[_]): Class[_] =
    if (clazz.getInterfaces.contains(target)) clazz else clazz.getInterfaces.map(i => findParentClassWith(i, target)).headOption.getOrElse(throw new ReflectException(s"Cannot find interface of $clazz which is a $target"))

}


case class Reflect[T](t: T) {


  def typedFields[F](implicit classTag: ClassTag[F]) = t.getClass.getFields.toList.filter(f => classTag.runtimeClass.isAssignableFrom(f.getType))
  def fieldValues[F: ClassTag]: List[F] = typedFields[F].map(_.get(t).asInstanceOf[F])
  def fieldNameAndValues[F: ClassTag]: List[(String, F)] = typedFields[F].map { f => (f.getName, f.get(t).asInstanceOf[F]) }

  def typedMethods[M](implicit classTag: ClassTag[M]) = t.getClass.getMethods.filter(m => classTag.runtimeClass.isAssignableFrom(m.getReturnType)).toList
  def zeroParamMethods[M: ClassTag]() = typedMethods[M].filter(_.getParameterCount == 0)
  def zeroParamMethodsNameAndValue[M: ClassTag]() = typedMethods[M].filter(_.getParameterCount == 0).map(m => (m.getName, m.invoke(t).asInstanceOf[M]))
}