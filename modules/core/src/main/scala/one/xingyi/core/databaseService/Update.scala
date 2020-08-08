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
