package org.xingyi.script.server
import scala.io.Source

trait Javascript extends CodeFragment

object Javascript {
  implicit def lensCodeMaker: LensCodeMaker[Javascript] = new JsMaker

  implicit def header: Header[Javascript] = name => Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("header.js")).mkString

  implicit def footer: Footer[Javascript] = () => ""
}
class JsMaker extends LensCodeMaker[Javascript] {

  def oneLens(name: String) = s"""lens("$name")"""

  def manyLens(names: List[String]) = names.map(name => oneLens(name)).mkString("compose(", ",", ");")

  override def apply(lens: LensDefn[_, _]): String = lens.names match {
    case one :: Nil => s"""function lens_${lens.name}(){ return ${oneLens(one)};}; """
    case names => s""""function lens_${lens.name}(){ return ${manyLens(names)}; }"""
  }
}
