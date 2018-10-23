/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample

import one.xingyi.core.cache.CacheFactory
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.language.{MicroserviceBuilder, MicroserviceComposers}
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult}
import one.xingyi.core.metrics.PutMetrics
import one.xingyi.core.monad.{Async, MonadCanFailWithException}
import one.xingyi.core.time.NanoTimeService
import one.xingyi.sample.domain.{Production, ProductionId, Programme, ProgrammeId}

import scala.language.higherKinds

class FnordSetup[M[_], J: JsonWriter : JsonParser, Wrapper[_, _], Fail]
(implicit
 val monad: MonadCanFailWithException[M, Fail],
 val async: Async[M],
 val failer: Failer[Fail],
 val cacheFactory: CacheFactory[M],
 val timeService: NanoTimeService,
 val logReqAndResult: LogRequestAndResult[Fail],
 val putMetrics: PutMetrics,
 val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
 val detailedLoggingForSR: DetailedLogging[ServiceResponse]
) extends PromotionServiceNames with MicroserviceBuilder[M, Fail] with MicroserviceComposers[M] {


  val production = function[ProductionId, Production]("production")(id => Production(s"from ${id.id}"))
  val program = function[ProgrammeId, Programme]("programme")(id => Programme(s"from ${id.id}"))

  val x = ProgrammeId.fromServiceRequest[M]
  val programmeEndpoint: ServiceRequest => M[Option[ServiceResponse]] = program |+| endpoint[ProgrammeId, Programme]("/programme", MatchesServiceRequest.idAtEnd(Get))
  val productionEndpoint: ServiceRequest => M[Option[ServiceResponse]] = production |+| endpoint[ProductionId, Production]("/production", MatchesServiceRequest.idAtEnd(Get))

  val microservice = chain(productionEndpoint, productionEndpoint)
}
