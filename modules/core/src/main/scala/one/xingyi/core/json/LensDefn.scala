package one.xingyi.core.json

import one.xingyi.core.reflection.{ClassTags, Reflect}

import scala.reflect.ClassTag


class ProjectionToLensDefns {
  def apply[T](projection: Projection[T]): List[LensDefn[_, _]] = {
    implicit val classTag: ClassTag[T] = projection.classTag
    projection match {
      case ObjectProjection(prototype, children@_*) =>
        println("Children is " + children)
        children.toList.flatMap {
          case (name, StringFieldProjection(get, set)) => List(LensDefn.string[T](name))
          case (name, o@ObjectFieldProjection(get, set)) => LensDefn.obj(name)(classTag, o.childClassTag) :: apply(o.projection)
          case (name, l@ListFieldProjection(get, set)) => LensDefn.list(name)(classTag, l.childClassTag) :: apply(l.projection)
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
