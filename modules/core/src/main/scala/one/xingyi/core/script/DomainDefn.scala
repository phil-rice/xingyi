/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.crypto.Digestor
import one.xingyi.core.json._
import one.xingyi.core.reflection.{ClassTags, Reflect}
import one.xingyi.core.script

import scala.language.implicitConversions
import scala.reflect.ClassTag
case class XingYiManualPath[A, B](prefix: String,  lensType: String, javascript: String, isList: Boolean = false)(implicit val classTag: ClassTag[A], val childClassTag: ClassTag[B]) {
  def makeManualLens(name: String) = ManualLensDefn[A, B](prefix + "_" + name, isList, javascript)
}

object DomainDefn {
  val xingyiHeaderPrefix = "application/xingyi."
}

case class InterfaceAndProjection[Shared, Domain](sharedOps: IXingYiSharedOps[IXingYiLens, Shared], projection: ObjectProjection[Shared, Domain])
object InterfaceAndProjection {
  implicit def tupleTo[Shared, Domain](tuple: (IXingYiSharedOps[IXingYiLens, Shared], ObjectProjection[Shared, Domain])) =
    InterfaceAndProjection(tuple._1, tuple._2)
}
abstract class DomainDefn[T: ClassTag](val sharedPackageName: String, val renderers: List[String], val interfacesToProjections: List[InterfaceAndProjection[_, _]] = List(),
                                       val manual: List[IXingYiSharedOps[XingYiManualPath, _]] = List())(implicit projectionToLensDefns: ProjectionToLensDefns) {
  def rootName: String = ClassTags.nameOf[T]
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


trait DomainDefnToDetails[T] extends (DomainDefn[T] => DomainDetails[T])

object DomainDefnToDetails {
  implicit def default[T](implicit javascript: HasLensCodeMaker[Javascript], scala: ToScalaCode[DomainDefn[T]]): DomainDefnToDetails[T] = { defn =>
    val scalaDetails = CodeDetails(scala(defn))
    val javascriptDetails = script.CodeDetails(javascript(defn))
    DomainDetails[T](defn.domainName, defn.packageName, defn.accepts, javascriptDetails.hash, defn.lens.map(_.name).toSet, Map(Javascript -> javascriptDetails, ScalaCode -> scalaDetails))
  }

}

case class CodeDetails(code: String)(implicit digestor: Digestor) {
  val hash = digestor(code)
}

case class DomainDetails[T](name: String, packageName: String, accept: String, codeHeader: String, lensNames: Set[String], code: Map[CodeFragment, CodeDetails]) {
  def normalisedLens = DomainDetails.stringsToString(lensNames)
  def isDefinedAt(lensNames: Set[String]) = lensNames.forall(this.lensNames.contains)
}
object DomainDetails {
  def stringsToString(set: Iterable[String]) = set.toList.sorted.mkString(".")
}


class CannotRespondToQuery(header: String, normalisedHeader: String, allowed: List[String]) extends RuntimeException(s"Header[$header] normalised[$normalisedHeader], allowed: ${allowed.mkString(";")}")
object DomainList {
  def stringToSet(s: String) = s.split("\\.").filterNot(_.isEmpty).toSet
}
case class DomainList[T](firstDomain: DomainDetails[T], restDomains: DomainDetails[T]*) {
  val domains = firstDomain :: restDomains.toList
  def accept(xingyiHeader: Option[String]) = {
    println(s"in Domain list$xingyiHeader")
    xingyiHeader match {
      case None => firstDomain
      case Some(header) =>
        if (!header.startsWith(DomainDefn.xingyiHeaderPrefix)) throw new RuntimeException(s"Must start with ${DomainDefn.xingyiHeaderPrefix} actually is header")
        val withoutPrefix = header.substring(DomainDefn.xingyiHeaderPrefix.length)
        println(s"in Domain list header $withoutPrefix")
        println(s"in Domain list split ${withoutPrefix.split("\\.").toList}")
        val set = DomainList.stringToSet(withoutPrefix)
        println(s"in Domain list set $set")
        domains.find(_.isDefinedAt(set)) match {
          case Some(foundDomain) =>
            println(s"in Domain list  found$xingyiHeader")
            foundDomain
          case None => println("throwing exception"); throw new CannotRespondToQuery(header, DomainDetails.stringsToString(set), domains.map(_.normalisedLens))
        }
    }
  }


}
