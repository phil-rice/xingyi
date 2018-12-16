/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.json.LensDefn
import one.xingyi.core.script.ScalaTrait.getClass

import scala.io.Source

trait ScalaTrait extends CodeFragment

object ScalaTrait {
  implicit def lensCodeMaker: LensCodeMaker[ScalaTrait] = new ScalaTraitMaker

  implicit def header: Header[ScalaTrait] = name =>
    s"""class $name(implicit xingYi:IXingYi) {
       |   def payload(json: String): Payload = xingYi.parse(json)
       |   def root: Lens[Payload,Person] = xingYi.objectLens("root")""".stripMargin

  implicit def renderer: Renderer[ScalaTrait] = name =>s"""   def render$name(domain: Domain): String = xingYi.render("$name",domain)"""

  implicit def footer: Footer[ScalaTrait] = () => "}"
}

class ScalaTraitMaker extends LensCodeMaker[ScalaTrait] {
  override def apply(lens: LensDefn[_, _]): String = {
    import lens._
    lens.b match {
      case "String" => s"""   def $name: Lens[$a,String] = xingYi.stringLens("$name") """
      case s if !lens.isList => s"""   def $name: Lens[$a,$b] = xingYi.objectLens("$name") """
      case s if lens.isList => s"""   def $name: Lens[$a,List[$b]] = xingYi.listLens("$name") """
    }
  }
}

trait ScalaDomain extends CodeFragment

object ScalaDomain extends ScalaDomain {
  val ignore = Set("String")

  implicit def hasLensCodeMaker: HasLensCodeMaker[ScalaDomain] = new HasLensCodeMaker[ScalaDomain] {
    override def apply[T](anyRef: DomainDefn[T]): String = {
      defns(anyRef).foldLeft(Set[String]())((set, d) => set ++ Set(d.a, d.b)).filterNot(ignore.contains).map(t =>
        s"""case class $t(mirror: Object) extends Domain
           |object $t {
           |   implicit def ${t}Maker: DomainMaker[$t] = $t.apply
           |}""".stripMargin
        //      s"trait $t"
      ).mkString("\n", "\n", "\n")
    }
  }

}

trait ScalaFull extends CodeFragment
object ScalaFull extends ScalaFull {
  implicit def hasLensCodeMaker(implicit scalaTrait: HasLensCodeMaker[ScalaTrait], scalaDomain: HasLensCodeMaker[ScalaDomain]): HasLensCodeMaker[ScalaFull] = new HasLensCodeMaker[ScalaFull] {
    override def apply[T](defn: DomainDefn[T]): String = {
      import defn._
      List(s"package $packageName",
        Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("header.scala")).mkString,
        scalaDomain(defn),
        scalaTrait(defn)).mkString("\n")
    }
  }
}