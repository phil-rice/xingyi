package org.xingyi.script.server

import one.xingyi.core.optics.Lens

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

trait Javascript extends CodeFragment

object Javascript {
  implicit def lensCodeMaker: LensCodeMaker[Javascript] = new JsMaker

  implicit def header: Header[Javascript] = name => ""

  implicit def footer: Footer[Javascript] = () => ""
}

trait ScalaTrait extends CodeFragment

object ScalaTrait {
  implicit def lensCodeMaker: LensCodeMaker[ScalaTrait] = new ScalaTraitMaker

  implicit def header: Header[ScalaTrait] = name => s"class $name(implicit xingYi:XingYi[Object]) {\n"

  implicit def footer: Footer[ScalaTrait] = () => "\n}"
}

trait ScalaDomain extends CodeFragment

object ScalaDomain {
  implicit def hasLensCodeMaker: HasLensCodeMaker[ScalaDomain] = new HasLensCodeMaker[ScalaDomain] {
    override def apply(anyRef: AnyRef): String = {
      defns(anyRef).foldLeft(Set[String]())((set, d) => set ++ Set(d.a, d.b)).map(t => s"trait $t").mkString("\n", "\n", "\n")
    }
  }

}


class JsMaker extends LensCodeMaker[Javascript] {

  def oneLens(name: String) = s"""lens("$name")"""

  def manyLens(names: List[String]) = names.map(name => oneLens(name)).mkString("compose(", ",", ");")

  override def apply(lens: LensDefn[_, _]): String = lens.names match {
    case one :: Nil => s""""function lens_${lens.name}(){ return ${oneLens(one)};}; """
    case names => s""""function lens_${lens.name}(){ return ${manyLens(names)}; }"""
  }
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

trait HasLensCodeMaker[L] {
  def defns(anyRef: AnyRef): Seq[LensDefn[_, _]] = {
    val methods = anyRef.getClass.getMethods.filter(field => classOf[LensDefn[_, _]].isAssignableFrom(field.getReturnType))
    methods.map(m => m.invoke(anyRef)).collect { case lens: LensDefn[_, _] => lens }
  }

  def apply(anyRef: AnyRef): String
}

class SimpleHasLensCodeMaker[L](implicit lensCodeMaker: LensCodeMaker[L], header: Header[L], footer: Footer[L]) extends HasLensCodeMaker[L] {

  def apply(anyRef: AnyRef): String = {
    defns(anyRef).map(lensCodeMaker).mkString(header(anyRef.getClass.getSimpleName), "\n", footer())
  }
}

object HasLensCodeMaker {
  implicit def maker[L: Header : Footer : LensCodeMaker]: HasLensCodeMaker[L] = new SimpleHasLensCodeMaker[L]
}


class XingYiServer {

}

