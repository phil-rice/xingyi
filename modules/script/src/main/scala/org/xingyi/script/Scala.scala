package org.xingyi.script

import scala.io.Source

trait ScalaTrait extends CodeFragment

object ScalaTrait {

  def makeFile(pack: String, domain: ScriptDomain)(implicit scalaDomain: HasLensCodeMaker[ScalaDomain], scalaTrait: HasLensCodeMaker[ScalaTrait]) =
    List(s"package $pack",
      Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("header.scala")).mkString,
      scalaDomain(domain),
      scalaTrait(domain)).mkString("\n")

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
      case s => s"""   def $name: Lens[$a,$b] = xingYi.objectLens("$name") """
    }
  }
}

trait ScalaDomain extends CodeFragment

object ScalaDomain {
  val ignore = Set("String")

  implicit def hasLensCodeMaker: HasLensCodeMaker[ScalaDomain] = new HasLensCodeMaker[ScalaDomain] {
    override def apply(anyRef: ScriptDomain): String = {
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

