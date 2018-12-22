/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import one.xingyi.core.optics.{DelegateLens, Lens}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.reflection.ClassTags

import scala.language.{higherKinds, implicitConversions}
trait IXingYiShared
trait IXingYiSharedOps[Lens[_, _], T]
trait IXingYiLens[A, B]


class ProofOfBinding[Shared, Domain]
object ProofOfBinding {
  implicit def proofOfLists[S, D] = new ProofOfBinding[List[S], List[D]]
}

object XingYiDomainStringLens {
  def stringLens[Shared, Domain](getter: Domain => String, setter: (Domain, String) => Domain)(implicit proof: ProofOfBinding[Shared, Domain]): XingYiDomainStringLens[Shared, Domain] =
    XingYiDomainStringLens(Lens(getter, setter))
}

case class XingYiDomainStringLens[Shared, Domain](lens: Lens[Domain, String])(implicit proof: ProofOfBinding[Shared, Domain]) extends DelegateLens[Domain, String] with IXingYiLens[Shared, String]
case class XingYiDomainObjectLens[Shared, SharedTarget, Domain, DomainTarget](lens: Lens[Domain, DomainTarget])
                                                                             (implicit sharedProof: ProofOfBinding[Shared, Domain], targetProof: ProofOfBinding[SharedTarget, DomainTarget])
  extends DelegateLens[Domain, DomainTarget] with IXingYiLens[Shared, SharedTarget]


import one.xingyi.core.json.JsonParserLanguage._

import scala.reflect.ClassTag
sealed trait Projection[T] {
  def classTag: ClassTag[T]
  def toJson(t: T): JsonValue
  def fromJson[J: JsonParser](j: J): T
  def fromJsonString[J](json: String)(implicit jsonParser: JsonParser[J]): T = fromJson(jsonParser(json))
  def walk[T1](fn: (List[String], FieldProjection[_, _]) => T1, prefix: List[String] = List()): Seq[T1]
}

case class ObjectProjection[T](prototype: T, children: (String, FieldProjection[T, _])*)(implicit val classTag: ClassTag[T]) extends Projection[T] {
  override def toJson(t: T): JsonValue = JsonObject(children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(t) }: _*)
  def fromJson[J: JsonParser](j: J): T =
    children.foldLeft(prototype) { case (acc, (name, fieldProjection)) => fieldProjection.setFromJson(acc, j \ name) }
  override def walk[T1](fn: (List[String], FieldProjection[_, _]) => T1, prefix: List[String]): Seq[T1] =
    children.flatMap { case (name, child) => child.walk(fn, prefix :+ name) }
}

sealed trait FieldProjection[T, Child] {
  def classTag: ClassTag[T]
  def childJson(t: T): JsonValue
  def set: (T, Child) => T
  def childFromJson[J: JsonParser](j: J): Child
  def setFromJson[J: JsonParser](t: T, j: J) = set(t, childFromJson(j))
  def walk[T1](fn: (List[String], FieldProjection[_, _]) => T1, prefix: List[String] = List()): List[T1] = List(fn(prefix, this))
  def nameOfT: String = classTag.runtimeClass.getSimpleName
  def isList: Boolean = false
}

object ObjectFieldProjection {
  def apply[Shared, SharedTarget, Domain:ClassTag, DomainTarget: ObjectProjection : ClassTag](lens: XingYiDomainObjectLens[Shared, SharedTarget, Domain, DomainTarget])
                                                                                    (implicit sharedProof: ProofOfBinding[Shared, Domain],
                                                                                     targetProof: ProofOfBinding[SharedTarget, DomainTarget]): ObjectFieldProjection[Domain, DomainTarget] =
    new ObjectFieldProjection[Domain, DomainTarget](lens.lens.get, lens.lens.set)

}
case class ObjectFieldProjection[T, Child](fn: T => Child, set: (T, Child) => T)(implicit val projection: ObjectProjection[Child], val classTag: ClassTag[T], val childClassTag: ClassTag[Child]) extends FieldProjection[T, Child] {
  override def childJson(t: T): JsonValue = JsonObject(projection.children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(fn(t)) }: _*)
  override def childFromJson[J: JsonParser](j: J): Child = projection.fromJson(j)
  override def toString: String = s"""ObjectFieldProjection[${ClassTags.nameOf[T]}, ${ClassTags.nameOf[Child]}](isList = $isList)"""
}

object ListFieldProjection {
  def apply[Shared, SharedTarget, Domain:ClassTag, DomainTarget: ObjectProjection : ClassTag](lens: XingYiDomainObjectLens[Shared, List[SharedTarget], Domain, List[DomainTarget]])
                                                                                    (implicit sharedProof: ProofOfBinding[Shared, Domain],
                                                                                     targetProof: ProofOfBinding[SharedTarget, DomainTarget]): ListFieldProjection[Domain, DomainTarget] =
    new ListFieldProjection[Domain, DomainTarget](lens.lens.get, lens.lens.set)
}
case class ListFieldProjection[T, Child](fn: T => List[Child], set: (T, List[Child]) => T)(implicit val projection: Projection[Child], val classTag: ClassTag[T], val childClassTag: ClassTag[Child]) extends FieldProjection[T, List[Child]] {
  override def childJson(t: T): JsonValue = JsonList(fn(t).map(projection.toJson))
  override def childFromJson[J: JsonParser](j: J): List[Child] = j.asListP[Child]
  override def isList: Boolean = true
  override def toString: String = s"""ListFieldProjection[${ClassTags.nameOf[T]}, ${ClassTags.nameOf[Child]}](isList = $isList)"""
}

object StringFieldProjection {
  def apply[Shared, Domain:ClassTag](lens: XingYiDomainStringLens[Shared, Domain])(implicit proof: ProofOfBinding[Shared, Domain]) = new StringFieldProjection[Domain](lens.lens.get, lens.lens.set)

}
case class StringFieldProjection[T](get: T => String, set: (T, String) => T)(implicit val classTag: ClassTag[T]) extends FieldProjection[T, String] {
  override def childJson(t: T): JsonValue = JsonString(get(t))
  override def childFromJson[J: JsonParser](j: J): String = j.as[String]
  override def toString: String = s"""StringFieldProjection[${ClassTags.nameOf[T]},String](isList = $isList)"""
}

