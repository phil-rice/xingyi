/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.tagless

import one.xingyi.core.cache._
import one.xingyi.core.http._
import one.xingyi.core.language.MicroserviceBuilder
import one.xingyi.core.logging._
import one.xingyi.core.metrics.PutMetrics
import one.xingyi.core.monad.{Async, MonadCanFailWithException}
import one.xingyi.core.time.NanoTimeService

import scala.language.{higherKinds, implicitConversions}

object TaglessLanguageLanguageForKleislis {
  def apply[M[_], Fail](implicit
                        async: Async[M],
                        monad: MonadCanFailWithException[M, Fail],
                        httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                        logReqAndResult: LogRequestAndResult[Fail],
                        timeService: NanoTimeService,
                        putMetrics: PutMetrics,
                        cacheFactory: CacheFactory[M],
                        failer: Failer[Fail],
                        responseParserFailer: ResponseParserFailer[Fail],
                        detailsLoggingForSR: DetailedLogging[ServiceResponse]) = new TaglessLanguageLanguageForKleislis[M, Fail]().NonFunctionalLanguageService()
}

class TaglessLanguageLanguageForKleislis[M[_], Fail] {
  type Kleisli[Req, Res] = Req => M[Res]

  case class NonFunctionalLanguageService(implicit
                                          protected val async: Async[M],
                                          protected val monad: MonadCanFailWithException[M, Fail],
                                          protected val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                          protected val logReqAndResult: LogRequestAndResult[Fail],
                                          protected val timeService: NanoTimeService,
                                          protected val putMetrics: PutMetrics,
                                          protected val cacheFactory: CacheFactory[M],
                                          protected val failer: Failer[Fail],
                                          protected val responseParserFailer: ResponseParserFailer[Fail],
                                          protected val detailsLoggingForSR: DetailedLogging[ServiceResponse]) extends
    TaglessLanguage[M, Kleisli] with MicroserviceBuilder[M, Fail] {
  }

}
