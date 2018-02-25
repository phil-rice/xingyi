package org.validoc.sample.domain

import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}

import scala.language.implicitConversions

case class ProgrammeId(id: String, bypassCache: Boolean) extends BypassCache

object ProgrammeId extends DomainCompanionQuery[ProgrammeId] {

  implicit object toRequestForProgrammeId extends ToServiceRequest[ProgrammeId] {
    override def apply(req: ProgrammeId): ServiceRequest = ServiceRequest(Get, Uri(s"/programme/${req.id}"))
  }

}

case class Programme(info: String)


object Programme extends DomainCompanionObject[ProgrammeId, Programme]