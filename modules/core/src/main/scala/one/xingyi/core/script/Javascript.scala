/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.json.{LensDefn, ManualLensDefn, SimpleLensDefn}

import scala.io.Source

trait Javascript extends CodeFragment {
  override def mediaType: MediaType = MediaType( "application/javascript")
}

object Javascript extends Javascript {
  implicit def lensCodeMaker: LensCodeMaker[Javascript] = new JsMaker

  implicit def header: Header[Javascript] = name => Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("header.js")).mkString

  implicit def renderer: Renderer[Javascript] = _ => ""

  implicit def footer: Footer[Javascript] = () => ""
}

class JsMaker extends LensCodeMaker[Javascript] {

  def oneLens(name: String) = s"""lens("$name")"""

  def manyLens(names: List[String]) = names.map(name => oneLens(name)).mkString("compose(", ",", ");")

  def lens(name: String, names: List[String]) = names match {
    case one :: Nil => s"""function $name(){ return ${oneLens(one)};}; """
    case names => s"""function $name(){ return ${manyLens(names)}; }"""
  }

  override def apply(lensDefn: LensDefn[_, _]): String =
    lensDefn match {
      case s: SimpleLensDefn[_, _] => lens(s.name, s.names)
      case s: ManualLensDefn[_, _] => s.javascript
    }
}
