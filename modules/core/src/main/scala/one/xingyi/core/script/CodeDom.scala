/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script
import one.xingyi.core.json.{IXingYiLens, IXingYiSharedOps, LensDefn}
import one.xingyi.core.reflection.Reflect
import one.xingyi.core.strings.Strings

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag

object ScalaCode extends ScalaCode {
  override def mediaType: MediaType = MediaType("application/scala")
}

trait ScalaCode extends CodeFragment

trait ToScalaCode[T] extends (T => String)
trait InterfaceToImplName {
  def opsInterface[L[_, _]](ops: IXingYiSharedOps[L, _]): String

  def opsServerSideImpl[L[_, _]](ops: IXingYiSharedOps[L, _]): String

  def impl(projection: Class[_]): String
}

object InterfaceToImplName {

  implicit object default extends InterfaceToImplName {
    override def opsServerSideImpl[L[_, _]](v1: IXingYiSharedOps[L, _]): String =
      impl(Reflect.findParentClassWith(v1.getClass, classOf[IXingYiSharedOps[L, _]]))

    override def impl(clazz: Class[_]): String = {
      Strings.removeOptional$(clazz.getSimpleName) match {
        //        case "String" => "String"
        case s if s.startsWith("I") && s.size > 1 => s.substring(1)
        case s => s + "Impl"
      }
    }

    override def opsInterface[L[_, _]](ops: IXingYiSharedOps[L, _]): String = {
      Reflect.findParentClassWith(ops.getClass, classOf[IXingYiSharedOps[L, _]]).getSimpleName
    }
  }

}


case class LensMethodCD(methodName: String, lensName: String, lensType: String)
object LensMethodCD {
  implicit def toScala: ToScalaCode[LensMethodCD] = { lmCd =>
    import lmCd._
    s"""   def $methodName = xingYi.$lensType("$lensName")""".stripMargin
  }
}
case class InterfaceCD(opInterfaceName: String, opClassName: String, typesThatWeLensTo: List[String], lensMethods: List[LensMethodCD])
object InterfaceCD {
  implicit def toScala(implicit methodToScala: ToScalaCode[LensMethodCD]): ToScalaCode[InterfaceCD] = { intCd =>
    import intCd._

    (s"""object $opClassName {
        |   implicit def hasHeader: IXingYiHeaderFor[$opClassName] =  () => List(${lensMethods.map(l => "\"" + l.lensName + "\"").mkString(",")})
        |}""" ::
      s"""class $opClassName(implicit val xingYi: IXingYi) extends $opInterfaceName[Lens, ${typesThatWeLensTo.mkString(",")}] {""" ::
      lensMethods.map(methodToScala) :::
      "}" ::
      List[String]()).map(_.stripMargin).mkString("\n")
  }
}
case class EntityCD(className: String, interfaceName: String)
object EntityCD {
  implicit def toScala: ToScalaCode[EntityCD] = { entCd =>
    import entCd._
    s"""case class $className (mirror: Object) extends Domain with $interfaceName
       |object $className {
       |  implicit object default extends DomainMaker[$className] {
       |    override def create(mirror: Object): $className = $className(mirror)
       |  }
       |}
    """.stripMargin
  }
}
case class DomainCD(packageName: String, sharedPackageName: String, domainName: String, imports: List[String], entities: List[EntityCD], interfaces: List[InterfaceCD])
object DomainCD {
  implicit def toScala(implicit interfaceToScala: ToScalaCode[InterfaceCD], entityToScala: ToScalaCode[EntityCD]): ToScalaCode[DomainCD] = { domCd =>
    import domCd._
    val lensNames = domCd.interfaces.flatMap(_.lensMethods.map(l => "\"" + l.lensName + "\"")).mkString(",")
    (s"package $packageName" ::
      imports.map(i => s"import $i").mkString("\n") ::
      s"""object $domainName extends ServerDomain{
         |  def lens=List($lensNames)
         |}""".stripMargin ::
      domCd.entities.map(entityToScala) ::: domCd.interfaces.map(interfaceToScala)).mkString("\n\n")
  }
}

trait DomainDefnToCodeDom {
  def apply[SharedE, DomainE: ClassTag](domainDefn: DomainDefn[SharedE, DomainE]): DomainCD
}

object DomainDefnToCodeDom {
  implicit def defaultToCodeDom[SharedE, DomainE: ClassTag] =
    new DefaultDomainDefnToCodeDom

  def imports(sharedPackageName: String) = List(
    sharedPackageName + "._",
    "one.xingyi.core.json.IXingYiHeaderFor",
    "one.xingyi.core.optics.Lens",
    "one.xingyi.core.script.{Domain,DomainMaker, IXingYi,ServerDomain}")
}

class DefaultDomainDefnToCodeDom(implicit interfaceToImplName: InterfaceToImplName) extends DomainDefnToCodeDom {
  def lensDefnToLensType[A, B](lensDefn: LensDefn[A, B]) = {
    import lensDefn._
    val targeta = interfaceToImplName.impl(lensDefn.classA.runtimeClass)
    val targetb = interfaceToImplName.impl(lensDefn.classB.runtimeClass)
    (b, isList) match {
      case ("String", false) => s"stringLens[$targeta]"
      case (_, false) => s"objectLens[$targeta,$targetb]"
      case (_, true) => s"listLens[$targeta,$targetb]"
    }
  }

  override def apply[SharedE, DomainE: ClassTag](domainDefn: DomainDefn[SharedE, DomainE]) = {
    val entityCDs = domainDefn.interfacesToProjections.map(_.projection).flatMap(_.allObjectProjections).distinct.map(_.sharedClassTag.runtimeClass).map(sharedClass => EntityCD(interfaceToImplName.impl(sharedClass), sharedClass.getName))
    def findXingYiInterfaceAnnotationClasses(clazz: Class[_]) = {
      clazz.getInterfaces.flatMap(_.getAnnotations.collect { case x: XingYiInterface => x }).headOption.getOrElse(throw new RuntimeException(s"could not find XingyiInterface annotation for $clazz")).clazzes().map(interfaceToImplName.impl).toList
    }
    val opCds = domainDefn.interfacesToProjections.map { case InterfaceAndProjection(projection, interface) =>
      InterfaceCD(
        interfaceToImplName.opsInterface(interface),
        interfaceToImplName.opsServerSideImpl(interface),
        findXingYiInterfaceAnnotationClasses(interface.getClass),
        Reflect(interface).zeroParamMethodsNameAndValue[IXingYiLens[_, _]].map { case (name, l) =>
          LensMethodCD(name, domainDefn.projectionLens(l).name, lensDefnToLensType(domainDefn.projectionLens(l)))
        })
    }

    DomainCD(domainDefn.packageName, domainDefn.sharedPackageName, domainDefn.domainName, DomainDefnToCodeDom.imports(domainDefn.sharedPackageName), entityCDs, opCds)
  }
}


