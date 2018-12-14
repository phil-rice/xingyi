package org.xingyi.script

import one.xingyi.core.reflection.ClassTags

import scala.reflect.ClassTag

case class LensDefn[A, B](name: String, names: List[String], isList: Boolean = false)(implicit val classA: ClassTag[A], val classB: ClassTag[B]) {
  val a = classA.runtimeClass.getSimpleName
  val b = classB.runtimeClass.getSimpleName
}

object LensDefn {
  def string[A: ClassTag, B: ClassTag](name: String): LensDefn[A, B] = LensDefn(ClassTags.lowerCaseNameOf[A] + "_" + name, List(name), false)
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

