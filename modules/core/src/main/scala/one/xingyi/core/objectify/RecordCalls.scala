package one.xingyi.core.objectify
import one.xingyi.core.http.{ServiceRequest, ServiceResponse}
import one.xingyi.core.json.{JsonList, JsonObject, JsonWriterLanguage, ToJsonLib}
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad

import scala.language.higherKinds


case class ResultWithRecordedCalls[T](t: T, calls: Seq[RecordedCall])

object ResultWithRecordedCalls {
  implicit def toJsonLib[T](implicit tToJson: ToJsonLib[T], recordedCallsToJson: ToJsonLib[RecordedCall]): ToJsonLib[ResultWithRecordedCalls[T]] =
    rc => JsonObject("original" -> tToJson(rc.t), "recorded" -> JsonList(rc.calls.map(recordedCallsToJson)))

}

case class RecordedCall(req: ServiceRequest, res: ServiceResponse)

object RecordedCall extends JsonWriterLanguage {
  implicit val default: InheritableThreadLocal[Seq[RecordedCall]] = new InheritableThreadLocal[Seq[RecordedCall]] {
    override def initialValue(): Seq[RecordedCall] = Seq()
  }

  implicit def toJsonLib(implicit serviceRequestToJson: ToJsonLib[ServiceRequest], serviceResponseToJson: ToJsonLib[ServiceResponse]): ToJsonLib[RecordedCall] =
    rc => JsonObject("request" -> serviceRequestToJson(rc.req), "response" -> serviceResponseToJson(rc.res))

}

trait RecordCallsKleisli[M[_], Fail] {
  def recordCalls(service: ServiceRequest => M[ServiceResponse])(implicit monad: Monad[M], recordedCalls: InheritableThreadLocal[Seq[RecordedCall]]): ServiceRequest => M[ServiceResponse] =
    req => service(req).map { res => recordedCalls.set(recordedCalls.get :+ RecordedCall(req, res)); res }

}

trait AddRecordedCalls[R]
