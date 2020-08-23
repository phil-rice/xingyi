/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.cheapApi

import one.xingyi.core.endpoint.{ChainKleisli, DisplayRecordedKleisli, EndpointKleisli}
import one.xingyi.core.http.Failer
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.logging.LoggingAdapter
import one.xingyi.core.monad.{Async, LiftFunctionKleisli, MonadCanFailWithException}
import one.xingyi.core.objectify.{ObjectifyKleisli, RecordCallsKleisli}

import scala.language.higherKinds

abstract class CheapApi[M[_] : Async, Fail: Failer , J: JsonParser : JsonWriter]
(implicit val monad: MonadCanFailWithException[M, Fail]) extends LiftFunctionKleisli[M] with MicroserviceComposers[M] with EndpointKleisli[M]
  with RecordCallsKleisli[M, Fail] with DisplayRecordedKleisli[M] with ChainKleisli[M] with ObjectifyKleisli[M,Fail]
