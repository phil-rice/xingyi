/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi

import one.xingyi.sample.domain._
import one.xingyi.sample.domain._

import scala.language.{higherKinds, postfixOps}


package object sample {

  type Service[M[_], Req, Res] = Req => M[Res]

  type MostPopularService[M[_]] = Service[M, Unit, MostPopular]
  type PromotionService[M[_]] = Service[M, Unit, List[Promotion]]

  type ProductionService[M[_]] = Service[M, ProductionId, Production]
  type ProgrammeService[M[_]] = Service[M, ProgrammeId, Programme]

  type EnrichPromotionService[M[_]] = Service[M, Unit, List[EnrichedPromotion]]
  type EnrichMostPopularService[M[_]] = Service[M, Unit, EnrichedMostPopular]



}
