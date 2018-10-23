/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain

import one.xingyi.core.cache.{CachableKey, ObjectId}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad
import one.xingyi.core.strings.Strings

import scala.language.{higherKinds, implicitConversions}

case class ProgrammeId(id: String, bypassCache: Boolean)

object ProgrammeId {

  implicit object toRequestForProgrammeId extends ToServiceRequest[ProgrammeId] {
    override def apply(req: ProgrammeId): ServiceRequest = ServiceRequest(Get, Uri(s"/programme/${req.id}"))
  }
  implicit def fromServiceRequest[M[_] : Monad] = new FromServiceRequest[M, ProgrammeId] {
    override def apply(v1: ServiceRequest) = ProgrammeId(Strings.lastSection("/")(v1.uri.path.asUriString), false).liftM
  }
  implicit def fromJsonLib[J: JsonParser]: FromJsonLib[J, ProgrammeId] = { json => ProgrammeId(json, false) }

  implicit object CachableKeyForProgrammeId extends CachableKey[ProgrammeId] {
    //    override def apply(v1: ProductionId) = v1.id
    override def id(req: ProgrammeId) = ObjectId(req)
    override def bypassCache(req: ProgrammeId) = req.bypassCache
  }

}

case class Programme(info: String)


object Programme {

  implicit object ToJsonForProgramme extends ToJson[Programme] {
    override def apply(v1: Programme) = s"""{programmeInfo: "${v1.info}"}"""
  }
  implicit def toJsonForProg[J: JsonWriter]: ToJsonLib[Programme] = { prog => JsonString(prog.info) }

  implicit def fromJson[J: JsonParser]: FromJsonLib[J, Programme] = { json => Programme(json) }

}
