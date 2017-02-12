package org.validoc.domain

import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, StringId}
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}
import org.validoc.utils.parser.ParserFinder

case class ProgrammeId(id: String) extends AnyVal

object ProgrammeId {

  implicit object ToRequestForProgrammeId extends ToServiceRequest[ProgrammeId] {
    override def apply(req: ProgrammeId): ServiceRequest =
      ServiceRequest(Get, Uri(s"someUri/${req.id}"))
  }

  implicit object CachableKeyForProgrammeId extends CachableKey[ProgrammeId] {
    override def id(req: ProgrammeId): Id = StringId(req.id)

    override def bypassCache(req: ProgrammeId): Boolean = false
  }

}

case class Programme(id: ProgrammeId, info: String)


object Programme {

  implicit object CachableResultForProgramme extends CachableResultUsingSucesses[Programme]

  implicit val parserFinderForProgramme = ParserFinder.always(_ => Programme(ProgrammeId("someId"), "someInfo"))
}