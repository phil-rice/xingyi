/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.crypto.Digestor
import one.xingyi.core.json._
import one.xingyi.core.reflection.{ClassTags, Reflect}
import one.xingyi.core.script

import scala.collection.immutable
import scala.language.implicitConversions
import scala.reflect.ClassTag

case class XingYiManualPath[A, B](prefix: String, lensType: String, javascript: String, isList: Boolean = false)(implicit val classTag: ClassTag[A], val childClassTag: ClassTag[B]) {
  def makeManualLens(name: String) = ManualLensDefn[A, B](prefix, isList, javascript)
}

object DomainDefn {
  val xingyiHeaderPrefix = "application/xingyi."
  val xingyiCodeSummaryMediaType = "application/json"

}

case class InterfaceAndProjection[Shared, Domain](sharedOps: IXingYiSharedOps[IXingYiLens, Shared], projection: ObjectProjection[Shared, Domain])

object InterfaceAndProjection {
  implicit def tupleTo[Shared, Domain](tuple: (IXingYiSharedOps[IXingYiLens, Shared], ObjectProjection[Shared, Domain])) =
    InterfaceAndProjection(tuple._1, tuple._2)
}


class DomainDefn[SharedE, DomainE: ClassTag](val sharedPackageName: String, val renderers: List[String],
                                             val interfacesToProjections: List[InterfaceAndProjection[_, _]] = List(),
                                             val manual: List[IXingYiSharedOps[XingYiManualPath, _]] = List())
                                            (implicit objectProjection: ObjectProjection[SharedE, DomainE], projectionToLensDefns: ProjectionToLensDefns) {
  def rootName: String = ClassTags.nameOf[DomainE]

  def packageName: String = getClass.getPackage.getName

  def domainName: String = getClass.getSimpleName

  val projectionLens: Map[IXingYiLens[_, _], LensDefn[_, _]] = interfacesToProjections.flatMap(x => projectionToLensDefns(x.projection)).distinct.toMap
  val manualLens: List[LensDefn[_, _]] = manual.flatMap(Reflect(_).zeroParamMethodsNameAndValue[XingYiManualPath[_, _]].map { case (name, path) => path.makeManualLens(name) }.toList)
  val lens = (projectionLens.values ++ manualLens).toList

  def accepts: String = DomainDefn.xingyiHeaderPrefix + DomainDetails.stringsToString(lens.map(_.name))

  override def toString: String =
    s"""${getClass.getSimpleName}(
       |renderers = ${renderers.mkString(",")}
       |projections =
       |${interfacesToProjections.mkString("\n")}
       |manual = ${manual.mkString(",")}
       |""".stripMargin
}


trait DomainDefnToDetails[SharedE, DomainE] extends (DomainDefn[SharedE, DomainE] => DomainDetails[SharedE, DomainE])

object DomainDefnToDetails {
  def apply[SharedE, DomainE: ClassTag](domainDefn: DomainDefn[SharedE, DomainE])(implicit domainDefnToDetails: DomainDefnToDetails[SharedE, DomainE]) = domainDefnToDetails(domainDefn)

  implicit def default[SharedE, DomainE](implicit javascript: HasLensCodeMaker[Javascript], scala: ToScalaCode[DomainDefn[SharedE, DomainE]]): DomainDefnToDetails[SharedE, DomainE] = { defn =>
    val scalaDetails = CodeDetails(scala(defn))
    val javascriptDetails = script.CodeDetails(javascript(defn))
    DomainDetails[SharedE, DomainE](defn.domainName, defn.packageName, defn.accepts, javascriptDetails.hash, defn.lens.map(_.name).toSet, Map(Javascript -> javascriptDetails, ScalaCode -> scalaDetails))
  }

}

case class CodeDetails(code: String)(implicit digestor: Digestor) {
  val hash = digestor(code)
}


case class DomainDetails[SharedE, DomainE](name: String, packageName: String, accept: String, codeHeader: String, lensNames: Set[String], code: Map[CodeFragment, CodeDetails]) {
  def normalisedLens = DomainDetails.stringsToString(lensNames)

  def isDefinedAt(lensNames: Set[String]) = lensNames.forall(this.lensNames.contains)
}

object DomainDetails {
  def stringsToString(set: Iterable[String]) = set.toList.sorted.mkString(".")
}


class CannotRespondToQuery(header: String, set: Set[String], normalisedHeader: String, failures: List[(String, Set[String], Set[String])]) extends
  RuntimeException(
    s"""Header[$header]
       | normalised[$normalisedHeader],
       | headerAsSet: ${set.toList.mkString(",")}
       | failures:
       | ${
      failures.map { case (name, allowed, failures) =>
        s"""Domain $name
           |   Allowed: ${allowed.toList.mkString(",")}
           |   Failed: ${failures.toList.mkString(",")}
           |""".stripMargin
      }.mkString(";")
    }""".stripMargin)

object DomainList {
  def stringToSet(s: String) = s.split(",").filterNot(_.isEmpty).toSet
}

case class DomainList[SharedE, DomainE](firstDomain: DomainDetails[SharedE, DomainE], restDomains: DomainDetails[SharedE, DomainE]*) {
  val domains = firstDomain :: restDomains.toList

  def accept(xingyiHeader: Option[String]) = {
    println(s"in Domain list$xingyiHeader")
    xingyiHeader match {
      case None => firstDomain
      case Some(header) if !header.contains(DomainDefn.xingyiHeaderPrefix) => firstDomain
      case Some(header) =>
        if (!header.startsWith(DomainDefn.xingyiHeaderPrefix)) throw new RuntimeException(s"Must start with ${DomainDefn.xingyiHeaderPrefix} actually is $header")
        val withoutPrefix = header.substring(DomainDefn.xingyiHeaderPrefix.length)
        val set = DomainList.stringToSet(withoutPrefix)
        domains.find(_.isDefinedAt(set)) match {
          case Some(foundDomain) => foundDomain
          case None =>
            val diff = set -- domains(0).lensNames
            val failures: List[(String, Set[String], Set[String])] = domains.map(d => (d.name, d.lensNames, set -- d.lensNames))
            throw new CannotRespondToQuery(header, set, DomainDetails.stringsToString(set), failures)
        }
    }
  }


}
