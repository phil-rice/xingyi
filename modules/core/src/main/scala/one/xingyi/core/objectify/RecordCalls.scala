/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
