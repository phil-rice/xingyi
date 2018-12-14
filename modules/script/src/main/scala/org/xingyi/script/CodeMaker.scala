package org.xingyi.script

import scala.reflect.ClassTag

case class LensDefn[A, B](name: String, names: List[String])(implicit val classA: ClassTag[A], val classB: ClassTag[B]) {
  val a = classA.runtimeClass.getSimpleName
  val b = classB.runtimeClass.getSimpleName
}


trait Header[L] extends (String => String)

trait Footer[L] extends (() => String)

trait LensCodeMaker[L] extends (LensDefn[_, _] => String)

object LensCodeMaker {
}

trait CodeFragment



trait HasLensCodeMaker[L <: CodeFragment] {
  def defns(anyRef: AnyRef): Seq[LensDefn[_, _]] = {
    val methods = anyRef.getClass.getMethods.filter(field => classOf[LensDefn[_, _]].isAssignableFrom(field.getReturnType))
    methods.map(m => m.invoke(anyRef)).collect { case lens: LensDefn[_, _] => lens }
  }

  def apply(anyRef: ScriptDomain): String
}

class SimpleHasLensCodeMaker[L<: CodeFragment](implicit lensCodeMaker: LensCodeMaker[L], header: Header[L], footer: Footer[L]) extends HasLensCodeMaker[L] {

  def apply(anyRef: ScriptDomain): String = {
    defns(anyRef).map(lensCodeMaker).mkString(header(anyRef.getClass.getSimpleName), "\n", footer())
  }
}

object HasLensCodeMaker {
  implicit def maker[L<: CodeFragment: Header : Footer : LensCodeMaker]: HasLensCodeMaker[L] = new SimpleHasLensCodeMaker[L]
}

