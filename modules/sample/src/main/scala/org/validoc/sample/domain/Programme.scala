package org.validoc.sample.domain

import org.validoc.utils.caching.{CachableKey, Id, StringId}
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}
import org.validoc.utils.metrics.{MetricValue, ReportData}

import scala.language.implicitConversions
import scala.util.Try

case class ProgrammeId(id: String) extends AnyVal

object ProgrammeId extends DomainCompanionObject[ProgrammeId] {

  implicit object toRequestForProgrammeId extends ToServiceRequest[ProgrammeId] {
    override def apply(req: ProgrammeId): ServiceRequest = ServiceRequest(Get, Uri(s"someUri/${req.id}"))
  }

  implicit object CachableKeyForProgrammeId extends CachableKey[ProgrammeId] {
    override def id(req: ProgrammeId): Id = StringId(req.id)

    override def bypassCache(req: ProgrammeId): Boolean = false
  }

}

case class Programme(id: ProgrammeId, info: String)


object Programme extends DomainCompanionObject[Programme] {

  implicit object ReportDataForProgramme extends ReportData[Programme] {
    override def apply(v1: String, v2: Try[Programme], v3: Long): Map[String, MetricValue] = Map()
  }

}