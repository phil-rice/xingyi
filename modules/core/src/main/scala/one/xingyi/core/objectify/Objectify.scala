package one.xingyi.core.objectify
import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.logging.DetailedLogging
import one.xingyi.core.monad.MonadCanFail

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
