/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.json._
import one.xingyi.core.strings.Strings


trait Header[L] extends (DomainDefn[_,_] => String)

trait Renderer[L] extends (String => String)

trait Footer[L] extends (() => String)

trait LensCodeMaker[L] extends (LensDefn[_, _] => String)

case class MediaType(s:String) extends AnyVal

trait CodeFragment{
  def mediaType:MediaType
  override def toString: String = Strings.removeOptional$(getClass.getSimpleName)
}


trait HasLensCodeMaker[L <: CodeFragment] {
  def defns[SharedE, DomainE](anyRef: DomainDefn[SharedE, DomainE]): List[LensDefn[_, _]] = anyRef.lens
//  {
//    val methods = anyRef.getClass.getMethods.filter(field => classOf[LensDefn[_, _]].isAssignableFrom(field.getReturnType)).toList
//    methods.map(m => m.invoke(anyRef)).collect { case lens: LensDefn[_, _] => lens }
//  }

  def apply[SharedE, DomainE](anyRef: DomainDefn[SharedE, DomainE]): String
}

class SimpleHasLensCodeMaker[L <: CodeFragment](implicit lensCodeMaker: LensCodeMaker[L], header: Header[L], render: Renderer[L], footer: Footer[L]) extends HasLensCodeMaker[L] {

  def apply[SharedE, DomainE](defn: DomainDefn[SharedE, DomainE]): String =
    (header(defn) :: defn.renderers.map(render) ::: defns(defn).map(lensCodeMaker) ::: List(footer())).mkString("\n")

}

object HasLensCodeMaker {
  implicit def maker[L <: CodeFragment : Header : Renderer : Footer : LensCodeMaker]: HasLensCodeMaker[L] = new SimpleHasLensCodeMaker[L]
}

