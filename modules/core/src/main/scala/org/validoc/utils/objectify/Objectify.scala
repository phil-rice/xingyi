package org.validoc.utils.objectify

import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.language.Language._

import scala.language.higherKinds
import scala.reflect.ClassTag

trait ObjectifyKleisli[M[_], Fail] {
  protected  implicit def monad: MonadCanFail[M, Fail]
  def objectify[Req: ClassTag, Res: ClassTag](http: ServiceRequest => M[ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseProcessor: ResponseParser[Fail, Req, Res]): Req => M[Res] =
    toRequest ~> http |==+> categoriser |=|> responseProcessor

}
