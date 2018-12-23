package one.xingyi.core.script

import one.xingyi.core.json._
import one.xingyi.core.optics.Lens
import one.xingyi.core.reflection.Reflect
import one.xingyi.core.script
import one.xingyi.core.strings.Strings

import scala.collection.immutable
import scala.reflect.ClassTag

object ScalaCode extends ScalaCode
trait ScalaCode extends CodeFragment

trait ToScalaCode[T] extends (T => String)

trait InterfaceToImplName {
  def opsInterface(ops: IXingYiSharedOps[IXingYiLens, _]): String
  def opsServerSideImpl(ops: IXingYiSharedOps[IXingYiLens, _]): String
  def impl(projection: Class[_]): String
}
object InterfaceToImplName {
  implicit object default extends InterfaceToImplName {
    override def opsServerSideImpl(v1: IXingYiSharedOps[IXingYiLens, _]): String =
      Strings.uppercaseFirst(Strings.removeOptional$(v1.getClass.getSimpleName) + "Impl")
    override def impl(clazz: Class[_]): String = {
      Strings.removeOptional$(clazz.getSimpleName) match {
        case s if s.startsWith("I") && s.size > 1 => s.substring(1)
        case s => s + "Impl"
      }
    }
    def findParentClassWith[T](clazz: Class[_], target: Class[_]): Class[_] = {
      if (clazz.getInterfaces.contains(target)) clazz else
        clazz.getInterfaces.map(i => findParentClassWith(i, target)).head
    }
    override def opsInterface(ops: IXingYiSharedOps[IXingYiLens, _]): String = {
      findParentClassWith(ops.getClass, classOf[IXingYiSharedOps[IXingYiLens, _]]).getSimpleName
    }
  }
}


object ToScalaCode {
  def lensDefnToXingYiMethod(lensAndDefn: IXingYiLensAndLensDefn)(implicit interfaceToImplName: InterfaceToImplName) =
    lensAndDefn.lensDefn match {
      case l if l.isList => s"objectLens[${interfaceToImplName.impl(lensAndDefn.lensDefn.classA.runtimeClass)},${interfaceToImplName.impl(lensAndDefn.lensDefn.classB.runtimeClass)}]"
      case l if l.b.equalsIgnoreCase("string") => s"stringLens[${interfaceToImplName.impl(lensAndDefn.lensDefn.classA.runtimeClass)}]"
      case l => s"objectLens[${interfaceToImplName.impl(lensAndDefn.lensDefn.classA.runtimeClass)},${interfaceToImplName.impl(lensAndDefn.lensDefn.classB.runtimeClass)}]"
    }
  implicit def makeScalaForIXingYiLensAndLensDefn(implicit interfaceToImplName: InterfaceToImplName): ToScalaCode[IXingYiLensAndLensDefn] = {
    case l@IXingYiLensAndLensDefn(name, lens, lensDefn: SimpleLensDefn[_, _]) =>
      s"""   def $name = xingYi.${lensDefnToXingYiMethod(l        )}("${lensDefn.name}")"""
  }
  implicit def makeScaleForInterface[Shared, SharedTarget](implicit interfaceToImplName: InterfaceToImplName, lensAndLensDefnToScala: ToScalaCode[IXingYiLensAndLensDefn]): ToScalaCode[InterfaceAndLens] = {
    case i@InterfaceAndLens(name, interface, list) =>
      val classStart = "class " + interfaceToImplName.opsServerSideImpl(interface) + "(implicit xingYi: IXingYi) extends " + interfaceToImplName.opsInterface(interface) + s"[Lens, $name]{"
      val middle = list.map(lensAndLensDefnToScala)
      val end = "}"
      (classStart :: middle ::: List(end)).mkString("\n")
  }

  //  case class Person(mirror: Object) extends Domain with IPerson
  //  object Person {
  //    implicit def PersonMaker: DomainMaker[Person] = Person.apply
  //    implicit def personOps(implicit xingYi: IXingYi) = new IPersonNameOps[Lens, Person] {
  //      override def name: Lens[Person, String] = xingYi.stringLens("iperson_name")
  //    }
  //  }


  implicit def makeScalaCode[T](implicit interfaceAndLensToScala: ToScalaCode[InterfaceAndLens], interfaceToImplName: InterfaceToImplName): ToScalaCode[DomainDefn[T]] = {
    domainDefn =>
      val classes = domainDefn.interfacesToProjections.map(_.projection).distinct.map {
        interface =>
          val impl = interfaceToImplName.impl(interface.sharedClassTag.runtimeClass)
          val classString = s"""class $impl (mirror: Object) extends ${interface.sharedClassTag.runtimeClass.getSimpleName}"""
          val objectString =
            s"""object $impl {
               |  implicit object default extends DomainMaker[$impl] {
               |    override def create(mirror: Object): $impl = $impl(mirror)
               |  }
               |}""".stripMargin
          List(classString, objectString).mkString("\n")
      }.mkString("\n")
      val ops: String = domainDefn.interfacesToProjections.map { case InterfaceAndProjection(interface, projection) =>
        InterfaceAndLens(interfaceToImplName.impl(projection.sharedClassTag.runtimeClass), interface, Reflect(interface).zeroParamMethodsNameAndValue[IXingYiLens[_, _]].
          map { case (name, l) =>
            println(s"looking for $name in interface $interface")
            IXingYiLensAndLensDefn(name, l, domainDefn.projectionLens.getOrElse(l, throw new RuntimeException(s"Cannot find name: $name l:$l in the projection. Keys are ${domainDefn.projectionLens.keys.toList.sortBy(_.toString)}")))
          })
      }.map(interfaceAndLensToScala).mkString("\n")
      List(classes, ops).mkString("\n\n\n")
  }


}

case class IXingYiLensAndLensDefn(name: String, lens: IXingYiLens[_, _], lensDefn: LensDefn[_, _])
case class InterfaceAndLens(nameOfImpl: String, interface: IXingYiSharedOps[IXingYiLens, _], list: List[IXingYiLensAndLensDefn])