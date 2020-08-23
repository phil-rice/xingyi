/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.MonadCanFail

import scala.language.higherKinds
//We are going to lift the service response into the Monad.  So it might 'fail' or throw an exception depending on the monad chosen
//the request is needed for validation. For example the request might have a security token and that has to be related to the response

case class RequestAndServiceResponse[Req](req: Req, serviceResponse: ServiceResponse)

trait ResponseCategoriser[Req] {
  def categorise[Fail](implicit failer: Failer[Fail]): Req => ServiceResponse => Either[Fail, RequestAndServiceResponse[Req]]
}

object ResponseCategoriser {
  def apply[Req]: ResponseCategoriser[Req] = new ResponseCategoriser[Req] {
    override def categorise[Fail](implicit failer: Failer[Fail]): Req => ServiceResponse => Either[Fail, RequestAndServiceResponse[Req]] =
      req => serviceResponse =>
        serviceResponse.status.code match {
          case x if x / 100 == 2 => Right(RequestAndServiceResponse(req, serviceResponse))
          case 404 => Left(failer.notFound(req, serviceResponse))
          case _ => Left(failer.unexpected(req, serviceResponse))
        }

  }

  implicit def default[Req] = apply[Req]

}
