package one.xingyi.sample.domain

import one.xingyi.core.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.core.functions.Monad
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.strings.Strings

import scala.language.{higherKinds, implicitConversions}

case class ProgrammeId(id: String, bypassCache: Boolean) extends BypassCache

object ProgrammeId extends DomainRequestCompanionQuery[ProgrammeId] {

  implicit object toRequestForProgrammeId extends ToServiceRequest[ProgrammeId] {
    override def apply(req: ProgrammeId): ServiceRequest = ServiceRequest(Get, Uri(s"/programme/${req.id}"))
  }
  implicit def fromServiceRequest[M[_] : Monad] = new FromServiceRequest[M, ProgrammeId] {
    override def apply(v1: ServiceRequest) = ProgrammeId(Strings.lastSection("/")(v1.body.map(_.s).getOrElse("")), false).liftM
  }
  implicit def fromJson[J: JsonParser]: FromJsonLib[J, ProgrammeId] = { json => ProgrammeId(json, false) }


}

case class Programme(info: String)


object Programme extends DomainResponseCompanionObject[ProgrammeId, Programme] {

  implicit object ToJsonForProgramme extends ToJson[Programme] {
    override def apply(v1: Programme) = s"""{programmeInfo: "${v1.info}"}"""
  }
  implicit def toJsonForProg[J: JsonWriter]: ToJsonLib[Programme] = { prog => JsonString(prog.info) }

  implicit def fromJson[J: JsonParser]: FromJsonLib[J, Programme] = { json => Programme(json) }

}