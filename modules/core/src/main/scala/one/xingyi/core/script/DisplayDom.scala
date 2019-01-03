package one.xingyi.core.script
import one.xingyi.core.json._

import scala.reflect.ClassTag
import one.xingyi.core.map.Maps._
import one.xingyi.core.reflection.Reflect

import scala.collection.immutable
case class LensMethodDD(lensName: String)
case class EntityDD(className: String, interfaceName: String)
case class MethodDD(methodName: String, urlPattern: String)
case class DomainDD(domainName: String, methodDDs: List[MethodDD], entities: List[EntityDD], interfaces: Map[String, List[LensMethodDD]], renderers: List[String])
object DisplayDom extends JsonWriterLanguage {
  implicit def toJson(implicit methodToJson: ToJsonLib[MethodDD]): ToJsonLib[DomainDD] =
    dd => JsonObject("name" -> dd.domainName,
      "methods" -> toListT(dd.methodDDs),
      "renderers" -> JsonList(dd.renderers.map(JsonString(_))))
}
case class DomainListDD(selected: DomainDD, domains: List[DomainDD])
object DomainListDD extends JsonWriterLanguage {
  implicit def toJson(implicit domainDDToJson: ToJsonLib[DomainDD]): ToJsonLib[DomainListDD] =
    list => JsonObject("selected" -> toT(list.selected), "seocndary" -> toListT(list.domains))
}

trait DomainAndMethodsToDisplayDom {
  def apply[SharedE, DomainE: ClassTag](domainDefn: DomainAndMethods[SharedE, DomainE]): DomainDD
}

object DomainAndMethodsToDisplayDom {
  implicit def toDisplayDom(implicit interfaceToImplName: InterfaceToImplName) = new DefaultDomainAndMethodsToDisplayDom
}

case class ListofDomainAndMethods[SharedE, DomainE](primary: DomainAndMethods[SharedE, DomainE], list: List[DomainAndMethods[SharedE, DomainE]])

trait DomainAndMethodListToDisplayDom {
  def apply[SharedE, DomainE: ClassTag](list: ListofDomainAndMethods[SharedE, DomainE])
                                       (implicit domainAndMethodsToDom: DomainAndMethodsToDisplayDom): DomainListDD
}
object DomainAndMethodListToDisplayDom {
  implicit def defaultToDisplayDom: DomainAndMethodListToDisplayDom = new DomainAndMethodListToDisplayDom {
    override def apply[SharedE, DomainE: ClassTag](listOfDomainAndMethods: ListofDomainAndMethods[SharedE, DomainE])
                                                  (implicit domainAndMethodsToDom: DomainAndMethodsToDisplayDom): DomainListDD = {
      import listOfDomainAndMethods._
      DomainListDD(domainAndMethodsToDom(primary), list.map(domainAndMethodsToDom.apply))
    }
  }
}


class DefaultDomainAndMethodsToDisplayDom(implicit interfaceToImplName: InterfaceToImplName) extends DomainAndMethodsToDisplayDom {
  override def apply[SharedE, DomainE: ClassTag](domainAndMethods: DomainAndMethods[SharedE, DomainE]): DomainDD = {
    val domainDefn = domainAndMethods.defn
    val entityDDs = domainDefn.interfacesToProjections.map(_.projection).
      flatMap(_.allObjectProjections).distinct.map(_.sharedClassTag.runtimeClass).
      map(sharedClass => EntityDD(interfaceToImplName.impl(sharedClass), sharedClass.getName))
    val lensDefns: List[LensDefn[_, _]] = domainDefn.interfacesToProjections.flatMap { case InterfaceAndProjection(projection, interface) =>
      Reflect(interface).zeroParamMethodsNameAndValue[IXingYiLens[_, _]].map(l => domainDefn.projectionLens(l._2))
    }
    val map = lensDefns.foldLeft(Map[String, List[LensMethodDD]]()) { case (acc, ld) => acc addToList (ld.classA.runtimeClass.getName -> LensMethodDD(ld.name)) }

    DomainDD(domainDefn.domainName, domainAndMethods.methodDatas.map(md => MethodDD(md.method.toString, md.urlPattern)), entityDDs, map, domainDefn.renderers)
  }
}

