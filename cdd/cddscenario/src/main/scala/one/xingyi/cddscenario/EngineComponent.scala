/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario

import one.xingyi.core.optics.Lens
import one.xingyi.core.reflection.SingleDefinedInSourceCodeAt
import one.xingyi.core.strings.HasTitle

trait HasEngineComponentData[T] extends (T => EngineComponentData)

object EngineComponentData {
  implicit object toTitleForEngineComponentData extends HasTitle[EngineComponentData] {
    override def apply(v1: EngineComponentData): Option[String] = v1.title
  }
  def toTitleL: Lens[EngineComponentData, Option[String]] = Lens(_.title, (d, t) => d.copy(title = t))
  def toCommentL: Lens[EngineComponentData, Option[String]] = Lens(_.comment, (d, c) => d.copy(comment = c))
  def toReferencesL: Lens[EngineComponentData, List[Reference]] = Lens(_.references, (d, r) => d.copy(references = r))
}
case class EngineComponentData(definedInSourceCodeAt: SingleDefinedInSourceCodeAt, title: Option[String], comment: Option[String] = None, whatsWrongWithMe: List[Exception] = List(), references: List[Reference] = List())


