package one.xingyi.core.script

import one.xingyi.core.json._
import one.xingyi.core.optics.Lens
import one.xingyi.core.reflection.{ClassTags, Reflect}
import one.xingyi.core.script
import one.xingyi.core.strings.Strings

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag

//TODO This file is a mess. Needs sorting out
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


object ToScalaCode {
  def lensDefnToXingYiMethod(lensAndDefn: IXingYiLensAndLensDefn)(implicit interfaceToImplName: InterfaceToImplName) =
    lensAndDefn.lensDefn match {
      case l if l.isList => s"listLens[${interfaceToImplName.impl(lensAndDefn.lensDefn.classA.runtimeClass)},${interfaceToImplName.impl(lensAndDefn.lensDefn.classB.runtimeClass)}]"
      case l if l.b.equalsIgnoreCase("string") => s"stringLens[${interfaceToImplName.impl(lensAndDefn.lensDefn.classA.runtimeClass)}]"
      case l => s"objectLens[${interfaceToImplName.impl(lensAndDefn.lensDefn.classA.runtimeClass)},${interfaceToImplName.impl(lensAndDefn.lensDefn.classB.runtimeClass)}]"
    }

  implicit def makeScalaForIXingYiLensAndLensDefn(implicit interfaceToImplName: InterfaceToImplName): ToScalaCode[IXingYiLensAndLensDefn] = {
    case l@IXingYiLensAndLensDefn(name, interface, lens, lensDefn: SimpleLensDefn[_, _]) =>
      s"""   def $name = xingYi.${lensDefnToXingYiMethod(l)}("${lensDefn.name}")"""
  }

  def findXingYiInterfaceAnnotationClasses(clazz: Class[_]) = {
    clazz.getInterfaces.flatMap(_.getAnnotations.collect { case x: XingYiInterface => x }).headOption.getOrElse(throw new RuntimeException(s"could not find XingyiInterface annotation for $clazz")).clazzes()
  }

  implicit def makeScaleForInterface[Shared, SharedTarget](implicit interfaceToImplName: InterfaceToImplName, lensAndLensDefnToScala: ToScalaCode[IXingYiLensAndLensDefn]): ToScalaCode[InterfaceAndLens] = {
    case i@InterfaceAndLens(name, interface, list) =>
      val s = findXingYiInterfaceAnnotationClasses(interface.getClass).map {
        interfaceToImplName.impl
      }.mkString(",")

      val classStart =
        s"""class ${interfaceToImplName.opsServerSideImpl(interface)}(implicit val xingYi: IXingYi) extends ${interfaceToImplName.opsInterface(interface)}[Lens, $s] {""".stripMargin
//      val headerField = s"""   def header=List(${list.map(x => "\"" + x.lensDefn.name + "\"").mkString(",")})"""

      val middle = list.map(lensAndLensDefnToScala)
      val end = "}"
      val objectHeader = "object " + interfaceToImplName.opsServerSideImpl(interface) + " {//" + interface.getClass
      val objectHeaderField = s"""   implicit def hasHeader: IXingYiHeaderFor[${interfaceToImplName.opsServerSideImpl(interface)}] =  () => List(${list.map(x => "\"" + x.lensDefn.name + "\"").mkString(",")})"""

      (objectHeader :: objectHeaderField :: end :: classStart ::  middle ::: List(end)).mkString("\n")
  }

  implicit def makeScalaCode[SharedE, DomainE](implicit interfaceAndLensToScala: ToScalaCode[InterfaceAndLens], interfaceToImplName: InterfaceToImplName): ToScalaCode[DomainDefn[SharedE, DomainE]] = {
    domainDefn =>
      val packageSring = s"package ${domainDefn.packageName}"
      val imports = s"import ${domainDefn.sharedPackageName}._\nimport one.xingyi.core.json.IXingYiHeaderFor\nimport one.xingyi.core.optics.Lens\nimport one.xingyi.core.script.{Domain,DomainMaker, IXingYi,ServerDomain}"
      val domainClasses = domainDefn.interfacesToProjections.map(_.projection).distinct.map {
        interface =>
          val impl = interfaceToImplName.impl(interface.sharedClassTag.runtimeClass)
          val classString =
            s"""case class $impl (mirror: Object) extends Domain with ${
              interface.sharedClassTag.runtimeClass.getSimpleName
            }"""
          val objectString =
            s"""object $impl {
               |  implicit object default extends DomainMaker[$impl] {
               |    override def create(mirror: Object): $impl = $impl(mirror)
               |  }
               |}""".stripMargin
          List(classString, objectString).mkString("\n")
      }.mkString("\n")
      val opsFromObjectProjections: String = domainDefn.interfacesToProjections.map {
        case InterfaceAndProjection( projection, interface) =>
          InterfaceAndLens(interfaceToImplName.impl(projection.sharedClassTag.runtimeClass), interface, Reflect(interface).zeroParamMethodsNameAndValue[IXingYiLens[_, _]].
            map {
              case (name, l) =>
                IXingYiLensAndLensDefn(name, interface, l, domainDefn.projectionLens.getOrElse(l, throw new RuntimeException(s"Cannot find name: $name l:$l in the projection. Keys are ${
                  domainDefn.projectionLens.keys.toList.sortBy(_.toString)
                }")))
            })
      }.map(interfaceAndLensToScala).mkString("\n")

      //      val implicitsForOps = domainDefn.manual.map { ops =>
      //        val methodNamesAndXingYiLines: immutable.Seq[(String, XingYiManualPath[_, _])] = Reflect(ops).zeroParamMethodsNameAndValue[XingYiManualPath[_, _]]()
      //        val objectHeader = "object " + interfaceToImplName.opsServerSideImpl(ops) + " {//" + ops.getClass
      //        val objectHeaderField = s"""   def hasHeader: IXingYiHeaderFor[${interfaceToImplName.opsServerSideImpl(ops)}] =  () => List(${methodNamesAndXingYiLines.map(x => "\"" + x._2.prefix + "\"").mkString(",")})"""
      //        List(objectHeader, objectHeaderField, "}").mkString("\n")
      //      }.mkString("\n")

      val opsFromLegacy = domainDefn.manual.map { ops =>
        val opClass = Reflect.findParentClassWith(ops.getClass, classOf[IXingYiSharedOps[XingYiManualPath, _]])
        val classes = opClass.getAnnotation(classOf[XingYiInterface]).clazzes().map(interfaceToImplName.impl).mkString(",")
        val header = "class " + interfaceToImplName.opsServerSideImpl(ops) + s"(implicit val xingYi: IXingYi) extends ${interfaceToImplName.opsInterface(ops)}[Lens, $classes]  {//" + ops.getClass
        val methodNamesAndXingYiLines: immutable.Seq[(String, XingYiManualPath[_, _])] = Reflect(ops).zeroParamMethodsNameAndValue[XingYiManualPath[_, _]]()

        //        val headerField = "  def header=" + methodNamesAndXingYiLines.map(_._2.prefix)
        val middle = methodNamesAndXingYiLines.map { case (name, value) =>
          val lensString = value.lensType match {
            case "stringLens" => s"${value.lensType}[${interfaceToImplName.impl(value.classTag.runtimeClass)}]"
            case _ => s"${value.lensType}[${interfaceToImplName.impl(value.classTag.runtimeClass)},${interfaceToImplName.impl(value.childClassTag.runtimeClass)}]"
          }
          s"""   def $name = xingYi.$lensString("${value.prefix}") """
        }.mkString("\n")
        val objectHeader = "object " + interfaceToImplName.opsServerSideImpl(ops) + " {//" + ops.getClass
        val objectHeaderField = s"""   implicit def hasHeader: IXingYiHeaderFor[${interfaceToImplName.opsServerSideImpl(ops)}] =  () => List(${methodNamesAndXingYiLines.map(x => "\"" + x._2.prefix + "\"").mkString(",")})"""
        List(objectHeader, objectHeaderField, "}", header, middle, "}").mkString("\n")
      }.mkString("\n")

      val domainCode = List(s"object ${domainDefn.domainName} extends ServerDomain{",
        s"   def lens=List(" + domainDefn.lens.map(_.name).mkString("\"", "\", \"", "\")"),
        "}").mkString("\n")
      List(packageSring, imports, domainClasses, opsFromObjectProjections, opsFromLegacy, domainCode).mkString("\n\n\n")
  }


}

case class IXingYiLensAndLensDefn(name: String, interface: IXingYiSharedOps[IXingYiLens, _], lens: IXingYiLens[_, _], lensDefn: LensDefn[_, _])

case class InterfaceAndLens(nameOfImpl: String, interface: IXingYiSharedOps[IXingYiLens, _], list: List[IXingYiLensAndLensDefn])