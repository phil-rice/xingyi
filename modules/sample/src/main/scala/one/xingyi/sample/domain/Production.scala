package one.xingyi.sample.domain

import one.xingyi.core.cache.{CachableKey, ObjectId}
import one.xingyi.core.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.core.functions.Monad
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.strings.Strings

import scala.language.{higherKinds, implicitConversions}
case class ProductionId(id: String, bypassCache: Boolean) extends BypassCache

object ProductionId extends DomainRequestCompanionQuery[ProductionId] {

  implicit object toRequestForProductionId extends ToServiceRequest[ProductionId] {
    override def apply(v1: ProductionId): ServiceRequest = ServiceRequest(Get, Uri(s"/production/${v1.id}"))
  }
  implicit object CachableKeyForProductionId extends CachableKey[ProductionId] {
    //    override def apply(v1: ProductionId) = v1.id
    override def id(req: ProductionId) = ObjectId(req)
    override def bypassCache(req: ProductionId) = false
  }
  implicit def fromServiceRequest[M[_] : Monad] = new FromServiceRequest[M, ProductionId] {
    override def apply(v1: ServiceRequest) = ProductionId(Strings.lastSection("/")(v1.body.map(_.s).getOrElse("")), false).liftM
  }

  import one.xingyi.core.json.JsonParserLanguage._
  implicit def fromJson[J: JsonParser]: FromJsonLib[J, ProductionId] = { json => ProductionId(jsonToString(json), false) }


}

case class Production(info: String)

object Production extends DomainResponseCompanionObject[ProductionId, Production] {
  implicit def fromJsonForProduction[J: JsonParser]: FromJsonLib[J, Production] = json => Production(json)

  implicit object ToJsonForPromotion extends ToJson[Production] {
    override def apply(v1: Production) = s"""{productionInfo: "${v1.info}"}"""
  }

  implicit def toJsonForPromotion[J: JsonWriter]: ToJsonLib[Production] = _.info
}