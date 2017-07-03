package org.validoc.sample.domain

import org.validoc.playJson.PlayJsonDomainObject
import org.validoc.utils.caching.{CachableKey, Id, StringId}
import org.validoc.utils.http.{Get, ServiceRequest, Uri}
import org.validoc.utils.metrics.{MetricValue, ReportData}
import play.api.libs.json.{Json, OFormat}

import scala.language.implicitConversions
import scala.util.Try

case class ProgrammeId(id: String) extends AnyVal

object ProgrammeId {
  implicit val modelFormat: OFormat[ProgrammeId] = Json.format[ProgrammeId]

  implicit def toRequestForProgrammeId(req: ProgrammeId) = ServiceRequest(Get, Uri(s"someUri/${req.id}"))

  implicit object CachableKeyForProgrammeId extends CachableKey[ProgrammeId] {
    override def id(req: ProgrammeId): Id = StringId(req.id)

    override def bypassCache(req: ProgrammeId): Boolean = false
  }

}

case class Programme(id: ProgrammeId, info: String)


object Programme extends PlayJsonDomainObject[Programme] {
  implicit val modelFormat: OFormat[Programme] = Json.format[Programme]

  implicit object ReportDataForProgramme extends ReportData[Programme] {
    override def apply(v1: String, v2: Try[Programme], v3: Long): Map[String, MetricValue] = Map()
  }

}