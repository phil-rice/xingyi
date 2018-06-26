/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain

import one.xingyi.core.aggregate.{Enricher, HasChildren}
import one.xingyi.core.cache.{CachableKey, UnitId}
import one.xingyi.core.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.core.json._
import one.xingyi.core.monad.Liftable
//needs to be here import io.circe.generic.auto._
import one.xingyi.core.http._
import one.xingyi.core.language.Language._

import scala.language.{higherKinds, implicitConversions}

case class MostPopularQuery(bypassCache: Boolean) extends BypassCache

object MostPopularQuery extends DomainRequestCompanionQuery[MostPopularQuery] {

  implicit object CachableKeyForMostPopularQuery extends CachableKey[MostPopularQuery] {
    //    override def apply(v1: MostPopularQuery) = ()
    override def id(req: MostPopularQuery) = UnitId
    override def bypassCache(req: MostPopularQuery) = req.bypassCache
  }


  implicit def toRequestForMostPopularQuery = new ToServiceRequest[MostPopularQuery] {
    override def apply(v1: MostPopularQuery): ServiceRequest = ServiceRequest(Get, Uri("/mostpopular"))
  }


  implicit def fromServiceRequest[M[_] : Liftable] = new fromServiceRequestForMostPopularQuery[M]

  class fromServiceRequestForMostPopularQuery[M[_] : Liftable] extends FromServiceRequest[M, MostPopularQuery] {
    override def apply(v1: ServiceRequest) = MostPopularQuery(false).liftM
  }

  implicit def fromHomePageQuery(h: HomePageQuery) = MostPopularQuery(h.bypassCache)

}


case class MostPopular(programmeIds: Seq[ProgrammeId])


object MostPopular extends DomainResponseCompanionObject[MostPopularQuery, MostPopular] {

  implicit object HasChildrenForMostPopular extends HasChildren[MostPopular, ProgrammeId] {
    override def apply(p: MostPopular): Seq[ProgrammeId] = p.programmeIds
  }
  implicit object ToJsonForMostPopular extends ToJson[MostPopular] {
    override def apply(v1: MostPopular) = s"""{id: [${v1.programmeIds.map(id => s""""$id"""").mkString(",")}]"""
  }

  implicit def fromJsonMostPopular[J: JsonParser](implicit forProgrammeId: FromJsonLib[J, ProgrammeId]): FromJsonLib[J, MostPopular] = json => MostPopular(json.asList[ProgrammeId])
}

case class EnrichedMostPopular(programmes: Seq[Programme])

object EnrichedMostPopular extends DomainResponseCompanionObject[MostPopularQuery, EnrichedMostPopular] {
  implicit object EnricherFor extends Enricher[MostPopularQuery, MostPopular, ProgrammeId, Programme, EnrichedMostPopular] {
    override def apply(v1: MostPopularQuery, v2: MostPopular, v3: Seq[(ProgrammeId, Programme)]) = EnrichedMostPopular(v3.map(_._2))
  }
  implicit def fromJsonForEMP[J: JsonParser](implicit forProgramme: FromJsonLib[J, Programme]): FromJsonLib[J, EnrichedMostPopular] = json => EnrichedMostPopular(json.asList[Programme])
  implicit def toJsonForEmp[J: JsonWriter](implicit forProgammer: ToJsonLib[Programme]): ToJsonLib[EnrichedMostPopular] = {emp => JsonObject("programmes" -> JsonList(emp.programmes.map(forProgammer)))}
}
