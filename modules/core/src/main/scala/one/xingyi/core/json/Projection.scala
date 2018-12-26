/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import one.xingyi.core.json
import one.xingyi.core.optics.{DelegateLens, Lens}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.reflection.ClassTags

import scala.language.{higherKinds, implicitConversions}
trait IXingYiShared
trait IXingYiSharedOps[Lens[_, _], T]
trait IXingYiLens[A, B]

case class JsonParsingException(msgs: List[String], cause: Exception) extends Exception(msgs.mkString("\n"))
object JsonParsingException {
  def wrap[X](msg: String, block: => X): X = try {
    block
  } catch {
    case JsonParsingException(msgs, cause) => throw new JsonParsingException(msg :: msgs, cause)
    case e: Exception => throw new JsonParsingException(List(msg), e)
  }
}
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
sealed trait Projection[Shared, Domain] {
  def proof: ProofOfBinding[Shared, Domain]
  def sharedClassTag: ClassTag[Shared]
  def domainClassTag: ClassTag[Domain]
  def toJson(t: Domain): JsonObject
  def fromJson[J: JsonParser](j: J): Domain
  def fromJsonString[J](json: String)(implicit jsonParser: JsonParser[J]): Domain = fromJson(jsonParser(json))
  def walk[T1](fn: (List[String], FieldProjection[_, _]) => T1, prefix: List[String] = List()): Seq[T1]
}

case class ObjectProjection[Shared, Domain](prototype: Domain, children: (String, FieldProjection[Domain, _])*)
                                           (implicit val sharedClassTag: ClassTag[Shared], val domainClassTag: ClassTag[Domain],
                                            val proof: ProofOfBinding[Shared, Domain]) extends Projection[Shared, Domain] {
  override def toJson(t: Domain): JsonObject = JsonObject(children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(t) }: _*)
  def fromJson[J: JsonParser](j: J): Domain =
    children.foldLeft(prototype) { case (acc, (name, fieldProjection)) =>
      JsonParsingException.wrap(s"Field $name for project $fieldProjection", fieldProjection.setFromJson(acc, j \ name))
    }
  override def walk[T1](fn: (List[String], FieldProjection[_, _]) => T1, prefix: List[String]): Seq[T1] =
    children.flatMap { case (name, child) => child.walk(fn, prefix :+ name) }
}

sealed trait FieldProjection[T, Child] {
  def domainClassTag: ClassTag[T]
  def childJson(t: T): JsonValue
  def set: (T, Child) => T
  def childFromJson[J: JsonParser](j: J): Child
  def setFromJson[J: JsonParser](t: T, j: J) = set(t, childFromJson(j))
  def walk[T1](fn: (List[String], FieldProjection[_, _]) => T1, prefix: List[String] = List()): List[T1] = List(fn(prefix, this))
  def nameOfT: String = domainClassTag.runtimeClass.getSimpleName
  def isList: Boolean = false
}

trait HasXingYiLens[Shared, SharedTarget] {
  def lens: IXingYiLens[Shared, SharedTarget]
}

case class ObjectFieldProjection[Shared, SharedTarget, Domain, DomainTarget](lens: XingYiDomainObjectLens[Shared, SharedTarget, Domain, DomainTarget])
                                                                            (implicit val projection: ObjectProjection[SharedTarget, DomainTarget], sharedProof: ProofOfBinding[Shared, Domain],
                                                                             targetProof: ProofOfBinding[SharedTarget, DomainTarget],
                                                                             val sharedClassTag: ClassTag[Shared],
                                                                             val sharedTargetClassTag: ClassTag[SharedTarget],
                                                                             val domainClassTag: ClassTag[Domain],
                                                                             val domainTargetClassTag: ClassTag[DomainTarget],
                                                                             val lensName: LensNameForJavascript[Shared, SharedTarget]) extends FieldProjection[Domain, DomainTarget]
  with DelegateLens[Domain, DomainTarget] with HasXingYiLens[Shared, SharedTarget] {
  override def childJson(t: Domain): JsonValue = JsonObject(projection.children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(lens(t)) }: _*)
  override def childFromJson[J: JsonParser](j: J): DomainTarget = projection.fromJson(j)
  override def toString: String = s"""ObjectFieldProjection[${ClassTags.nameOf[Domain]}, ${ClassTags.nameOf[DomainTarget]}](isList = $isList)"""
}

case class ListFieldProjection[Shared, SharedTarget, Domain, DomainTarget](lens: XingYiDomainObjectLens[Shared, List[SharedTarget], Domain, List[DomainTarget]])
                                                                          (implicit val projection: Projection[SharedTarget, DomainTarget],
                                                                           sharedProof: ProofOfBinding[Shared, Domain],
                                                                           targetProof: ProofOfBinding[SharedTarget, DomainTarget],
                                                                           val sharedClassTag: ClassTag[Shared],
                                                                           val sharedTargetClassTag: ClassTag[SharedTarget],
                                                                           val domainClassTag: ClassTag[Domain],
                                                                           val domainTargetClassTag: ClassTag[DomainTarget],
                                                                           val lensName: LensNameForJavascript[Shared, SharedTarget]
                                                                          ) extends FieldProjection[Domain, List[DomainTarget]]
  with DelegateLens[Domain, List[DomainTarget]] with HasXingYiLens[Shared, List[SharedTarget]] {
  override def childJson(t: Domain): JsonValue = JsonList(lens(t).map(projection.toJson))
  override def childFromJson[J: JsonParser](j: J): List[DomainTarget] = j.asListP[SharedTarget, DomainTarget]
  override def isList: Boolean = true
  override def toString: String = s"""ListFieldProjection[${ClassTags.nameOf[Domain]}, ${ClassTags.nameOf[DomainTarget]}](isList = $isList)"""
}

case class StringFieldProjection[Shared, Domain](lens: XingYiDomainStringLens[Shared, Domain])
                                                (implicit proof: ProofOfBinding[Shared, Domain],
                                                 val sharedClassTag: ClassTag[Shared],
                                                 val domainClassTag: ClassTag[Domain],
                                                 val lensName: LensNameForJavascript[Shared, String]) extends FieldProjection[Domain, String]
  with HasXingYiLens[Shared, String]
  with DelegateLens[Domain, String] {
  override def childJson(t: Domain): JsonValue = JsonString(lens.get(t))
  override def childFromJson[J: JsonParser](j: J): String = j.as[String]
  override def toString: String = s"""StringFieldProjection[${ClassTags.nameOf[Domain]},String](isList = $isList)"""
}

case class StringField[Domain](lens: Lens[Domain, String])(implicit val domainClassTag: ClassTag[Domain]) extends FieldProjection[Domain, String] with DelegateLens[Domain, String] {
  override def childJson(t: Domain): JsonValue = JsonString(lens.get(t))
  override def childFromJson[J: JsonParser](j: J): String = j.as[String]
  override def toString: String = s"""StringFieldProjection[${ClassTags.nameOf[Domain]},String](isList = $isList)"""
}

