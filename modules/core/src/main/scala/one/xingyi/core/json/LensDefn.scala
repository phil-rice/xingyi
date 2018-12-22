package one.xingyi.core.json

import one.xingyi.core.reflection.{ClassTags, Reflect}

import scala.reflect.ClassTag


class ProjectionToLensDefns {
  def apply[Shared, Domain](projection: Projection[Shared, Domain]): List[LensDefn[_, _]] = {
    implicit val sharedClassTag: ClassTag[Shared] = projection.sharedClassTag
    implicit val domainClassTag: ClassTag[Domain] = projection.domainClassTag
    implicit val proof = projection.proof

    projection match {
      case ObjectProjection(prototype, children@_*) =>
        println("Children is " + children)
        val x = children
        children.toList.flatMap {
          case (name, s: StringFieldProjection[_, _]) => List(LensDefn.string(name)(s.sharedClassTag))
          case (name, o@ObjectFieldProjection(get, set)) => LensDefn.obj(name)(o.sharedClassTag, o.sharedTargetClassTag) :: apply(o.projection)
          case (name, l: ListFieldProjection[_, _, _, _]) => LensDefn.list(name)(l.sharedClassTag, l.sharedTargetClassTag) :: apply(l.projection)
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
case class ManualLensListLensDefn[A: ClassTag, B: ClassTag](name: String, names: List[String], isList: Boolean) extends LensDefn[A, B]
case class ManualLensDefn[A: ClassTag, B: ClassTag](name: String, isList: Boolean, javascript: String) extends LensDefn[A, B]


object LensDefn {
  def string[A: ClassTag](name: String): LensDefn[A, String] = SimpleLensDefn(ClassTags.lowerCaseNameOf[A] + "_" + name, List(name), false)
  def obj[A: ClassTag, B: ClassTag](names: String*): LensDefn[A, B] = SimpleLensDefn(ClassTags.lowerCaseNameOf[A] + "_" + ClassTags.lowerCaseNameOf[B], names.toList, false)
  def list[A: ClassTag, B: ClassTag](names: String*): LensDefn[A, B] = {
    println(s"Making LensDefn.list(${ClassTags.nameOf[A]},${ClassTags.nameOf[B]})")
    SimpleLensDefn(ClassTags.lowerCaseNameOf[A] + "_" + ClassTags.lowerCaseNameOf[B] + "_list", names.toList, true)
  }
}
