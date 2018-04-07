package one.xingyi.utils.objectify

import one.xingyi.utils.functions.MonadCanFail
import one.xingyi.utils.http._
import one.xingyi.utils.language.Language._
import one.xingyi.utils.logging.DetailedLogging

import scala.language.higherKinds
import scala.reflect.ClassTag


trait ObjectifyKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFail[M, Fail]
  protected implicit def failer: Failer[Fail]
  protected implicit def detailsLoggingForSR: DetailedLogging[ServiceResponse]

  def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: ServiceRequest => M[ServiceResponse])
                                                               (implicit toRequest: ToServiceRequest[Req],
                                                                categoriser: ResponseCategoriser[Req],
                                                                responseProcessor: ResponseParser[Req, Res]): Req => M[Res] =
    toRequest ~> http  |=|+> categoriser.categorise[Fail]  |=|> responseProcessor.parse[Fail]
}
