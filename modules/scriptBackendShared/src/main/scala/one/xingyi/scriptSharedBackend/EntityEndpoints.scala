/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptSharedBackend

import one.xingyi.core.builder.{CopyWithNewId, HasId}
import one.xingyi.core.endpoint.{ChainKleisli, EndpointKleisli, MatchesServiceRequest}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.logging._
import one.xingyi.core.monad._
import one.xingyi.core.script._

import scala.language.higherKinds

class EntityEndpoints[M[_] : Async, Fail, J: JsonParser : JsonWriter, SharedE, DomainE: Links : EntityPrefix]
(implicit val monad: MonadCanFailWithException[M, Fail], val logReqAndResult: LogRequestAndResult[Fail],
 val failer: Failer[Fail],
 editEntityFailer: EditEntityRequestFailer[Fail],
 entityStoreFailer: IEntityStoreFailer[Fail],
 entityStore: IEntityStore[M, DomainE],
 hasId: HasId[DomainE, String], copyWithId: CopyWithNewId[DomainE, String], loggingAdapter: LoggingAdapter,
 domainList: DomainList[SharedE, DomainE],
 projection: ObjectProjection[SharedE, DomainE]) extends LiftFunctionKleisli[M] with ChainKleisli[M, Fail] with EndpointKleisli[M] with MicroserviceComposers[M] {

  import projection.proof


  val entityCodeMaker = new EntityCodeMaker[M, Fail, SharedE, DomainE]
  val entityMaker = new EntityMaker[M, Fail, SharedE, DomainE](List(Get, Post, Put))

  val keepAlive: ServiceRequest => M[Option[ServiceResponse]] = function[ServiceRequest, ServiceResponse]("keepalive")(sr => ServiceResponse("Alive")) |+| endpoint[ServiceRequest, ServiceResponse]("/ping", MatchesServiceRequest.fixedPath(Method("get")))

  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(
    entityMaker.detailsEndpoint,
    chain(entityCodeMaker.endpoints: _*),
    entityMaker.newEntity(id => copyWithId(id, projection.prototype).liftM),
    entityMaker.getEndpoint(entityStore.getEntity),
    entityMaker.editEndpoint(entityStore.putEntity),
    keepAlive)
}

