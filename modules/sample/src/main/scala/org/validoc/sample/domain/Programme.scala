package org.validoc.sample.domain

import org.validoc.utils.domain.{BypassCache, DomainCompanionObject, DomainCompanionQuery}
import org.validoc.utils.functions.Monad
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.strings.Strings

import scala.language.higherKinds
import scala.language.implicitConversions
import org.validoc.utils.language.Language._

case class ProgrammeId(id: String, bypassCache: Boolean) extends BypassCache

object ProgrammeId extends DomainCompanionQuery[ProgrammeId] {

  implicit object toRequestForProgrammeId extends ToServiceRequest[ProgrammeId] {
    override def apply(req: ProgrammeId): ServiceRequest = ServiceRequest(Get, Uri(s"/programme/${req.id}"))
  }
  implicit def fromServiceRequest[M[_] : Monad] = new FromServiceRequest[M, ProgrammeId] {
    override def apply(v1: ServiceRequest) = ProgrammeId(Strings.lastSection("/")(v1.body.map(_.s).getOrElse("")), false).liftM
  }


}

case class Programme(info: String)


object Programme extends DomainCompanionObject[ProgrammeId, Programme]{

  implicit object ToJsonForProgramme extends ToJson[Programme] {
    override def apply(v1: Programme) = s"""{programmeInfo: "${v1.info}"}"""
  }

}