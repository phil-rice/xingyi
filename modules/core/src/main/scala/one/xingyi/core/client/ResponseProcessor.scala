/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.client

import one.xingyi.core.http.ServiceResponse
import one.xingyi.core.json.FromJson
import one.xingyi.core.monad.MonadWithException

import scala.language.higherKinds

case class UnexpectedResponse(sr: ServiceResponse) extends Exception(sr.toString)
case class UrlDidntResponse(uri: String, cause: Throwable) extends Exception(uri, cause)

trait ResponseProcessor[M[_], Req, Res] extends (Req => ServiceResponse => M[Res])

abstract class AbstractResponseProcess[M[_], Req, Res](implicit monad: MonadWithException[M]) extends ResponseProcessor[M, Req, Res] {
  override def apply(req: Req): ServiceResponse => M[Res] = sr => sr.status.code / 100 match {
    case 2 => process200(req, sr)
    case _ => processOther(req, sr)
  }
  def process200(req: Req, sr: ServiceResponse): M[Res]
  def processOther(req: Req, sr: ServiceResponse): M[Res] = monad.exception(UnexpectedResponse(sr))
}
object ResponseProcessor {
  implicit def default[M[_], Req, Res](implicit monad: MonadWithException[M], fromJson: FromJson[Res]): ResponseProcessor[M, Req, Res] = new DefaultResponseProcess[M, Req, Res]
}
class DefaultResponseProcess[M[_], Req, Res](implicit monad: MonadWithException[M], fromJson: FromJson[Res]) extends AbstractResponseProcess[M, Req, Res] {
  override def process200(req: Req, sr: ServiceResponse): M[Res] = monad.liftM(fromJson(sr.body.s))
}
