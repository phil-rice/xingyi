package org.xingyi.script

import scala.reflect.ClassTag

case class LensDefn[A, B](name: String, names: List[String])(implicit val classA: ClassTag[A], val classB: ClassTag[B]) {
  val a = classA.runtimeClass.getSimpleName
  val b = classB.runtimeClass.getSimpleName
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
  implicit def maker[L <: CodeFragment : Header : Renderer: Footer : LensCodeMaker]: HasLensCodeMaker[L] = new SimpleHasLensCodeMaker[L]
}

