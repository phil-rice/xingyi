package one.xingyi.core.transformService

import one.xingyi.core.http.{FromServiceRequest, ServiceRequest, ServiceResponse, ToServiceResponse}
import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.language.MonadFunctionLanguage._
import one.xingyi.core.monad.Monad

import scala.language.higherKinds
trait RequestTransformer[ToReq, FromReq] extends (ToReq => FromReq)
trait ResponseTransformer[ToRes, FromRes] extends (FromRes => ToRes)

/** Used in legacy migration tasks.  */
trait TransformKleisli[M[_]] {

  def replace[OReq, ORes, NewReq, NewRes](service: NewReq => M[NewRes])
                                         (implicit monad: Monad[M],
                                          fromServiceRequest: FromServiceRequest[M, OReq],
                                          toServiceResponse: ToServiceResponse[OReq, ORes],
                                          reqTransformer: RequestTransformer[OReq, NewReq],
                                          resTransformer: ResponseTransformer[ORes, NewRes]): ServiceRequest => M[ServiceResponse] =
    fromServiceRequest |==> (reqTransformer ~> service |=> resTransformer |=+> toServiceResponse)

}
