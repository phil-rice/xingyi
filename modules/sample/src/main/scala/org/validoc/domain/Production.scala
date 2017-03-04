package org.validoc.domain

import org.validoc.utils.ToServiceRequest
import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, StringId}
import org.validoc.utils.http.{Get, ServiceRequest, Uri}
import org.validoc.utils.parser.ParserFinder


case class ProductionId(id: String) extends AnyVal

object ProductionId{
  implicit def toRequestForHomePageQueryForProductionId (req: ProductionId)=     ServiceRequest(Get, Uri(s"someId/${req.id}"))
  implicit object CachableKeyForProductionId extends CachableKey[ProductionId] {
    override def id(req: ProductionId): Id = StringId(req.id)

    override def bypassCache(req: ProductionId): Boolean = false
  }
}

case class Production(id: ProductionId, info: String)

object Production {

  implicit object CachableResultForProduction extends CachableResultUsingSucesses[Production]

  implicit val parserFinderForProduction = ParserFinder.always(_ => Production(ProductionId("someId"), "someInfo"))
}