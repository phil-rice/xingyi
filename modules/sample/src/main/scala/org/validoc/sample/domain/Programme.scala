package org.validoc.sample.domain

import org.validoc.playJson.PlayJsonDomainObject
import org.validoc.utils.caching.{CachableKey, Id, StringId}
import org.validoc.utils.http.{Get, ServiceRequest, Uri}
import play.api.libs.json.{Json, OFormat}
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


object Programme extends PlayJsonDomainObject[Programme]{
  implicit val modelFormat: OFormat[Programme] = Json.format[Programme]


}