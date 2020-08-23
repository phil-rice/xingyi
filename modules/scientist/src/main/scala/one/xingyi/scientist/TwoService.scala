/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scientist

import one.xingyi.core.language.MonadLanguage._
import one.xingyi.core.monad.MonadCanFail

import scala.language.higherKinds

trait TwoServiceFailer[M[_], Fail] {
  def nothingSelected: Fail

  def requiredServiceNotPresent(details: String): Fail
}

object TwoServiceFailer {
  implicit def twoServiceFailerDefault[M[_], Throwable]: TwoServiceFailer[M, Throwable] = new TwoServiceFailer[M, Throwable] {
    override def nothingSelected: Throwable = new RuntimeException("Nothing selected").asInstanceOf[Throwable]

    override def requiredServiceNotPresent(details: String) = new RuntimeException("Required service not present: " + details).asInstanceOf[Throwable]
  }
}

trait TwoServiceProcessor[M[_], Fail, Req, Res] {
  def selectAndTransformRequests: Req => M[(Option[Req], Option[Req])]

  def postProcessResults: TwoServiceMerger[M, Req, Res]
}

trait TwoServiceKlesli[M[_], Fail] {

  implicit class TwoServicePimper[Req, Res](tuple: (Req => M[Res], Req => M[Res]))
                                           (implicit twoServiceProcessor: TwoServiceProcessor[M, Fail, Req, Res],
                                            monad: MonadCanFail[M, Fail],
                                            twoServiceFailer: TwoServiceFailer[M, Fail]) {
    def asOneService: Req => M[Res] = TwoService(twoServiceProcessor, tuple._1, tuple._2)
  }
}

case class TwoService[M[_], Fail, Req, Res](
                                             twoServiceProcessor: TwoServiceProcessor[M, Fail, Req, Res],
                                             service1: Req => M[Res],
                                             service2: Req => M[Res]
                                           )(implicit monad: MonadCanFail[M, Fail], twoServiceFailer: TwoServiceFailer[M, Fail]) extends (Req => M[Res]) {
  def optionallyCallAndPrefixReq(service: Req => M[Res], optReq: Option[Req]): Option[(Req, M[Res])] = optReq.map(req => (req, service(req)))

  def apply(req: Req): M[Res] = {
    twoServiceProcessor.selectAndTransformRequests(req).flatMap { case (optReq1, optReq2) =>
      twoServiceProcessor.postProcessResults(req,
        optionallyCallAndPrefixReq(service1, optReq1),
        optionallyCallAndPrefixReq(service2, optReq2))
    }
  }
}
