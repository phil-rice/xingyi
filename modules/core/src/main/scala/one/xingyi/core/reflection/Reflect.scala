/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
