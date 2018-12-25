package one.xingyi.core.json

import one.xingyi.core.reflection.{ClassTags, Reflect}

import scala.reflect.ClassTag


class ProjectionToLensDefns {
  def apply[Shared, Domain](projection: Projection[Shared, Domain]): List[(IXingYiLens[_, _], LensDefn[_, _])] = {
    implicit val sharedClassTag: ClassTag[Shared] = projection.sharedClassTag
    implicit val domainClassTag: ClassTag[Domain] = projection.domainClassTag
    implicit val proof = projection.proof

    projection match {
      case ObjectProjection(prototype, children@_*) =>
        val x = children
        children.toList.flatMap {
          case (name, s: StringField[_]) => List()
          case (name, s: StringFieldProjection[_, _]) => List((s.lens, LensDefn.string(name)(s.sharedClassTag, s.lensName)))
          case (name, o: ObjectFieldProjection[_, _, _, _]) => (o.lens, LensDefn.obj(name)(o.sharedClassTag, o.sharedTargetClassTag, o.lensName)) :: apply(o.projection)
          case (name, l: ListFieldProjection[_, _, _, _]) => (l.lens, LensDefn.list(name)(l.sharedClassTag, l.sharedTargetClassTag, l.lensName)) :: apply(l.projection)
        }
    }
  }
}
object ProjectionToLensDefns {
  implicit val projectionToLensDefns: ProjectionToLensDefns = new ProjectionToLensDefns
}


sealed abstract class LensDefn[A, B](implicit val classA: ClassTag[A], val classB: ClassTag[B]) {
  def name: String
  def isList: Boolean
  val a = classA.runtimeClass.getSimpleName
  val b = classB.runtimeClass.getSimpleName
}
case class SimpleLensDefn[A: ClassTag, B: ClassTag](name: String, names: List[String], isList: Boolean = false) extends LensDefn[A, B]
case class ManualLensDefn[A: ClassTag, B: ClassTag](name: String, isList: Boolean, javascript: String) extends LensDefn[A, B]


trait LensNameForJavascript[A, B] {
  def apply(name: String, isList: Boolean)(implicit classTagA: ClassTag[A], classTagB: ClassTag[B]): String
}
object LensNameForJavascript {
  def removedFirstLetterIfI(s: String) = if (s.startsWith("I")) s.substring(1) else s
  def objectName[T](implicit classTag: ClassTag[T]) = removedFirstLetterIfI(classTag.runtimeClass.getSimpleName)

  implicit def default[A, B] = new LensNameForJavascript[A, B] {
    override def apply(name: String, isList: Boolean)(implicit classTagA: ClassTag[A], classTagB: ClassTag[B]) = {
      (List("lens", objectName[A](classTagA), name, objectName[B](classTagB)).mkString("_") + (if (isList) "List" else "")).toLowerCase
    }
  }
}

object LensDefn {
  def string[A: ClassTag](name: String)(implicit lensNameForJavascript: LensNameForJavascript[A, String]): LensDefn[A, String] =
    SimpleLensDefn(lensNameForJavascript(name, false), List(name), false)
  def obj[A: ClassTag, B: ClassTag](name: String)(implicit lensNameForJavascript: LensNameForJavascript[A, B]): LensDefn[A, B] =
    SimpleLensDefn(lensNameForJavascript(name, false), List(name), false)
  def list[A: ClassTag, B: ClassTag](name: String)(implicit lensNameForJavascript: LensNameForJavascript[A, B]): LensDefn[A, B] = {
    SimpleLensDefn(lensNameForJavascript(name, true), List(name), true)
  }
}
