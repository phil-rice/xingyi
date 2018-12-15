package org.xingyi.script

import one.xingyi.core.json._
import one.xingyi.core.reflection.ClassTags
import org.xingyi.script
import sun.font.GlyphLayout.LayoutEngine

import scala.reflect.ClassTag

class ProjectionToLensDefns {
  def apply[T](projection: Projection[T])(implicit classTag: ClassTag[T]): Seq[LensDefn[_, _]] = {
    projection match {
      case ObjectProjection(children@_*) =>
        println("Children is " + children)
        children.flatMap {
          case (name, StringFieldProjection(fn)) => List(LensDefn.string[T](name))
          case (name, o@ObjectFieldProjection(fn)) => LensDefn.obj(name)(classTag, o.classTag) :: apply(o.projection)(o.classTag).toList
          case (name, l@ListFieldProjection(fn)) => LensDefn.list(name)(classTag,l.classTag) :: apply(l.projection)(l.classTag).toList
        }
    }
  }
}
object ProjectionToLensDefns {
  implicit val projectionToLensDefns: ProjectionToLensDefns = new ProjectionToLensDefns
}


case class LensDefn[A, B](name: String, names: List[String], isList: Boolean = false)(implicit val classA: ClassTag[A], val classB: ClassTag[B]) {
  val a = classA.runtimeClass.getSimpleName
  val b = classB.runtimeClass.getSimpleName
}

object LensDefn {
  def string[A: ClassTag](name: String): LensDefn[A, String] = LensDefn(ClassTags.lowerCaseNameOf[A] + "_" + name, List(name), false)
  def obj[A: ClassTag, B: ClassTag](names: String*): LensDefn[A, B] = LensDefn(ClassTags.lowerCaseNameOf[A] + "_" + ClassTags.lowerCaseNameOf[B], names.toList, false)
  def list[A: ClassTag, B: ClassTag](names: String*): LensDefn[A, B] = LensDefn(ClassTags.lowerCaseNameOf[A] + "_" + ClassTags.lowerCaseNameOf[B] + "_list", names.toList, true)
}

trait Header[L] extends (String => String)

trait Renderer[L] extends (String => String)

trait Footer[L] extends (() => String)

trait LensCodeMaker[L] extends (LensDefn[_, _] => String)

trait CodeFragment


trait HasLensCodeMaker[L <: CodeFragment] {
  def defns(anyRef: AnyRef): List[LensDefn[_, _]] = {
    val methods = anyRef.getClass.getMethods.filter(field => classOf[LensDefn[_, _]].isAssignableFrom(field.getReturnType)).toList
    methods.map(m => m.invoke(anyRef)).collect { case lens: LensDefn[_, _] => lens }
  }

  def apply(anyRef: ScriptDomain): String
}

class SimpleHasLensCodeMaker[L <: CodeFragment](implicit lensCodeMaker: LensCodeMaker[L], header: Header[L], render: Renderer[L], footer: Footer[L]) extends HasLensCodeMaker[L] {

  def apply(anyRef: ScriptDomain): String =
    (header(anyRef.name) :: anyRef.renderers.map(render) ::: defns(anyRef).map(lensCodeMaker) ::: List(footer())).mkString("\n")

}

object HasLensCodeMaker {
  implicit def maker[L <: CodeFragment : Header : Renderer : Footer : LensCodeMaker]: HasLensCodeMaker[L] = new SimpleHasLensCodeMaker[L]
}

