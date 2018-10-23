/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample

import one.xingyi.sample.domain.{Promotion, PromotionQuery}
import one.xingyi.tagless.TaglessLanguage
import one.xingyi.sample.domain._
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http.{Failer, Get, ServiceRequest, ServiceResponse}
import one.xingyi.core.json.JsonWriter
import one.xingyi.core.monad.MonadCanFail

import scala.language.{higherKinds, implicitConversions}

//class BillboardSetup[M[_], Fail](interpreter: MicroserviceBuilder[M, Fail] with MicroserviceComposers[M])
class BillboardSetup[ M[_],J: JsonWriter, Wrapper[_, _], Fail](interpreter: TaglessLanguage[M, Wrapper])
                                               (implicit
                                                monadCanFail: MonadCanFail[M, Fail],
                                                failer: Failer[Fail]
                                               ) extends PromotionServiceNames {

  import interpreter._

  implicit def toProductionId(id: Int) = domain.ProductionId(id.toString, false)
  val billboard = function[PromotionQuery, Promotion]("promotions")(query => Promotion(List(1, 2, 3)))
  val billboardEndpoint: Wrapper[ServiceRequest, Option[ServiceResponse]] = billboard |+| endpoint[PromotionQuery, Promotion]("/billboard", MatchesServiceRequest.fixedPath(Get))

  val microservice = chain(billboardEndpoint)
}
