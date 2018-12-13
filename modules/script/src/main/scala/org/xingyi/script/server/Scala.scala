package org.xingyi.script.server

trait ScalaTrait extends CodeFragment

object ScalaTrait {
  implicit def lensCodeMaker: LensCodeMaker[ScalaTrait] = new ScalaTraitMaker

  implicit def header: Header[ScalaTrait] = name =>
    s"""class $name(implicit xingYi:XingYi[Object]) {
       |   def root: Lens[Payload,Person] = xingYi.objectLens("root")
       |""".stripMargin

  implicit def footer: Footer[ScalaTrait] = () => "\n}"
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
  implicit def hasLensCodeMaker: HasLensCodeMaker[ScalaDomain] = new HasLensCodeMaker[ScalaDomain] {
    override def apply(anyRef: ScriptDomain): String = {
      defns(anyRef).foldLeft(Set[String]())((set, d) => set ++ Set(d.a, d.b)).map(t => s"trait $t").mkString("\n", "\n", "\n")
    }
  }

}

