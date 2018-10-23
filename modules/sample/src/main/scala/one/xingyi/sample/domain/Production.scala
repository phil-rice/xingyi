/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain

import one.xingyi.core.cache.{CachableKey, ObjectId}
import one.xingyi.core.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad
import one.xingyi.core.strings.Strings

import scala.language.{higherKinds, implicitConversions}
case class ProductionId(id: String, bypassCache: Boolean) extends BypassCache

object ProductionId extends DomainRequestCompanionQuery[ProductionId] {

  implicit object ToJsonLibForProductionId extends ToJsonLib[ProductionId] {
    override def apply(v1: ProductionId): JsonValue = JsonString(v1.id)
  }
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
