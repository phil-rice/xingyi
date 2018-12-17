/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.crypto.Digestor
import one.xingyi.core.json.LensDefn
import one.xingyi.core.reflection.ClassTags
import one.xingyi.core.script

import scala.reflect.ClassTag

abstract class DomainDefn[T: ClassTag](
                                        val renderers: List[String] = List(),
                                        val lens: Seq[LensDefn[_, _]] = List()) {
  def rootName: String = ClassTags.nameOf[T]
  def packageName: String = getClass.getPackage.getName
  def domainName: String = getClass.getSimpleName


}


trait DomainDefnToDetails[T] extends (DomainDefn[T] => DomainDetails[T])

object DomainDefnToDetails {
  implicit def default[T](implicit javascript: HasLensCodeMaker[Javascript], scala: HasLensCodeMaker[ScalaFull]): DomainDefnToDetails[T] =
    defn => DomainDetails[T](defn.domainName, defn.packageName, Map(Javascript -> script.CodeDetails(javascript(defn)), ScalaFull -> CodeDetails(scala(defn))))

}

case class CodeDetails(code: String)(implicit digestor: Digestor) {
  val hash = digestor(code)
}

case class DomainDetails[T](name: String, packageName: String, code: Map[CodeFragment, CodeDetails])
