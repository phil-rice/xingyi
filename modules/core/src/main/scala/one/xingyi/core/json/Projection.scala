/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json
import one.xingyi.core.strings.Strings

import scala.reflect.ClassTag
import JsonParserLanguage._
sealed trait Projection[T] {
  def classTag: ClassTag[T]
  def toJson(t: T): JsonValue
  def fromJson[J: JsonParser](j: J): T
}

case class ObjectProjection[T](prototype: T, children: (String, FieldProjection[T, _])*)(implicit val classTag: ClassTag[T]) extends Projection[T] {
  override def toJson(t: T): JsonValue = JsonObject(children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(t) }: _*)
  def fromJson[J: JsonParser](j: J): T =
    children.foldLeft(prototype) { case (acc, (name, fieldProjection)) => fieldProjection.setFromJson(acc, j \ name) }

}

sealed trait FieldProjection[T, Child] {
  def childJson(t: T): JsonValue
  def set: (T, Child) => T
  def childFromJson[J: JsonParser](j: J): Child
  def setFromJson[J: JsonParser](t: T, j: J) = set(t, childFromJson(j))
}

case class ObjectFieldProjection[T, Child](fn: T => Child, set: (T, Child) => T)(implicit val projection: ObjectProjection[Child], val classTag: ClassTag[Child]) extends FieldProjection[T, Child] {
  override def childJson(t: T): JsonValue = JsonObject(projection.children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(fn(t)) }: _*)
  override def childFromJson[J: JsonParser](j: J): Child = projection.fromJson(j)
}

case class ListFieldProjection[T, Child](fn: T => List[Child], set: (T, List[Child]) => T)(implicit val projection: Projection[Child], val classTag: ClassTag[Child]) extends FieldProjection[T, List[Child]] {
  override def childJson(t: T): JsonValue = JsonList(fn(t).map(projection.toJson))
  override def childFromJson[J: JsonParser](j: J): List[Child] = j.asListP[Child]
}
case class StringFieldProjection[T](get: T => String, set: (T, String) => T) extends FieldProjection[T, String] {
  override def childJson(t: T): JsonValue = JsonString(get(t))
  override def childFromJson[J: JsonParser](j: J): String = j.as[String]
}
