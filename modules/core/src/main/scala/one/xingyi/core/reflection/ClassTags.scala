/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.reflection

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.List
import scala.reflect.ClassTag

trait ClassTags {
  def clazz[T](implicit classTag: ClassTag[T]) = classTag.runtimeClass
  def nameOf[T](implicit classTag: ClassTag[T]) = classTag.runtimeClass.getSimpleName
  def lowerCaseNameOf[T](implicit classTag: ClassTag[T]) = classTag.runtimeClass.getSimpleName.toLowerCase

  def isA[T](t: Any)(implicit classTag: ClassTag[T]) = classTag.runtimeClass.isAssignableFrom(t.getClass)

  def collectAll[T: ClassTag](i: Seq[Any]): Seq[T] = i.collect { case r if isA[T](r) => r.asInstanceOf[T] }

  implicit class AnyPimperForClassTags[T](a: T) {
    def is[T1: ClassTag]: Boolean = isA[T1](a)
  }

  implicit class IterablePimperForClassTags(s: Seq[Any]) {
    def collectAs[T: ClassTag]: Seq[T] = collectAll[T](s)
  }

}

object ClassTags extends ClassTags
