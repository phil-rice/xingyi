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