/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.databaseService.DatabaseServiceImplicits._
import one.xingyi.core.http._
import one.xingyi.core.json.JsonLanguage._
import one.xingyi.core.json._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Monad
import one.xingyi.core.strings.Strings

import scala.language.higherKinds
case class UpdateRequest(name: String, map: Map[String, String] = Map()) extends DatabaseRequest

object UpdateRequest {
  implicit def fromServiceRequestForStoredProcedureUpdate[M[_] : Monad, J](implicit parser: JsonParser[J]): FromServiceRequest[M, UpdateRequest] =
    sr => UpdateRequest(Strings.lastSection("/")(sr.path.path), parser(sr.body.getOrElse(throw DatabaseRequest.needException(s"No Body in ServiceRequest $sr")).s).as[Map[String, String]]).liftM[M]

  implicit def toServiceRequestFoStoredProcedureUpdateRequest[J](implicit jsonWriter: JsonWriter[J], mapToJson: ToJsonLib[Map[String, String]]): ToServiceRequest[UpdateRequest] =
    req => ServiceRequest(Post, Uri(s"/update/${req.name}"), Seq(), Some(Body(jsonWriter(mapToJson(req.map)))))

}

case class UpdateResponse(result: Int)

object UpdateResponse {
  def toResult(res: UpdateResponse) : Int= res.result
  implicit def toJsonForStoredProcedureUpdateResponse: ToJsonLib[UpdateResponse] = up => JsonObject("count" -> JsonInt(up.result))
  implicit def fromJsonForStoredProcedureUpdateResponse[J: JsonParser]: FromJsonLib[J, UpdateResponse] = json => UpdateResponse(json \ "count")
}
