package one.xingyi.sample.domain

import one.xingyi.utils.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.utils.functions.Monad
import one.xingyi.utils.http._
import one.xingyi.utils.json.ToJson
import one.xingyi.utils.language.Language._
import one.xingyi.utils.strings.Strings

import scala.language.{higherKinds, implicitConversions}

case class ProgrammeId(id: String, bypassCache: Boolean) extends BypassCache

object ProgrammeId extends DomainRequestCompanionQuery[ProgrammeId] {

  implicit object toRequestForProgrammeId extends ToServiceRequest[ProgrammeId] {
    override def apply(req: ProgrammeId): ServiceRequest = ServiceRequest(Get, Uri(s"/programme/${req.id}"))
  }
  implicit def fromServiceRequest[M[_] : Monad] = new FromServiceRequest[M, ProgrammeId] {
    override def apply(v1: ServiceRequest) = ProgrammeId(Strings.lastSection("/")(v1.body.map(_.s).getOrElse("")), false).liftM
  }


}

case class Programme(info: String)


object Programme extends DomainResponseCompanionObject[ProgrammeId, Programme]{

  implicit object ToJsonForProgramme extends ToJson[Programme] {
    override def apply(v1: Programme) = s"""{programmeInfo: "${v1.info}"}"""
  }

}