/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import one.xingyi.core.builder.HasId
import one.xingyi.core.http.{Body, FromServiceRequest, ServiceRequest}
import one.xingyi.core.json.{JsonParser, Projection}
import one.xingyi.core.monad.{Monad, MonadCanFailWithException}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds


case class EditEntityRequest[P](entity: P, acceptHeader: Option[String], host: String)
// aha need to be able to make from projection
object EditEntityRequest {
  implicit def hasId[P](implicit hasId: HasId[P, String]): HasId[EditEntityRequest[P], String] = req => hasId(req.entity)

  implicit def hasHost[P]: HasHost[EditEntityRequest[P]] = _.host

  implicit def fromServiceRequest[M[_], Fail, J: JsonParser, SharedP, DomainP]
  (implicit monad: MonadCanFailWithException[M, Fail], failer: EditEntityRequestFailer[Fail], hasId: HasId[DomainP, String], projection: Projection[SharedP, DomainP]): FromServiceRequest[M, EditEntityRequest[DomainP]] = { sr =>
    val name = Strings.lastSection("/")(sr.uri.path.path)
    failer.failOrUseHost(sr) { host =>
      failer.failOrUseBody(sr) { body =>
        val newEntity: DomainP = projection.fromJsonString(body)
        if (name != hasId(newEntity)) monad.fail(failer.failIdDoesntMatch(name, sr)) else
          monad.liftM(EditEntityRequest(newEntity, sr.header("accept"), host))
      }
    }
  }

  implicit def toContentType[P]: ToContentType[EditEntityRequest[P]] = req => req.acceptHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)

}
