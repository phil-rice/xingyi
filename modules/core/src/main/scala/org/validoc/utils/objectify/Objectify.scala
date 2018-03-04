package org.validoc.utils.objectify

import org.validoc.utils._
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.tagless.CommonForKleislis

import scala.language.higherKinds
import scala.reflect.ClassTag

trait ObjectifyKleisli[M[_], Fail] extends CommonForKleislis[M] {
  implicit def monad: MonadCanFail[M, Fail]
  def objectify[Req: ClassTag, Res: ClassTag](http: Kleisli[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseProcessor: ResponseParser[Fail, Req, Res]): Req => M[Res] =
    toRequest ~> http |==+> categoriser |=|> responseProcessor

}
