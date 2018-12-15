/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
  def defns(anyRef: ScriptDomain): List[LensDefn[_, _]] = anyRef.lens.toList
//  {
//    val methods = anyRef.getClass.getMethods.filter(field => classOf[LensDefn[_, _]].isAssignableFrom(field.getReturnType)).toList
//    methods.map(m => m.invoke(anyRef)).collect { case lens: LensDefn[_, _] => lens }
//  }

  def apply(anyRef: ScriptDomain): String
}

class SimpleHasLensCodeMaker[L <: CodeFragment](implicit lensCodeMaker: LensCodeMaker[L], header: Header[L], render: Renderer[L], footer: Footer[L]) extends HasLensCodeMaker[L] {

  def apply(anyRef: ScriptDomain): String =
    (header(anyRef.name) :: anyRef.renderers.map(render) ::: defns(anyRef).map(lensCodeMaker) ::: List(footer())).mkString("\n")

}

object HasLensCodeMaker {
  implicit def maker[L <: CodeFragment : Header : Renderer : Footer : LensCodeMaker]: HasLensCodeMaker[L] = new SimpleHasLensCodeMaker[L]
}

