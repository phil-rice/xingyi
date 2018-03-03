package org.validoc.utils.objectify

import org.validoc.utils._
import org.validoc.utils.functions.Monad
import org.validoc.utils.http._
import org.validoc.utils.tagless.CommonForKleislis

import scala.language.higherKinds
import scala.reflect.ClassTag

trait ObjectifyKleisli[M[_]] extends CommonForKleislis[M] {
  implicit def monad: Monad[M]
  def objectify[Req: ClassTag, Res: ClassTag](http: Kleisli[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseProcessor[M, Req, Res]) =
    toRequest ~> http |=+> categoriser |==> responseProcessor
}
