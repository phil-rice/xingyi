package org.validoc.utils

import scala.concurrent.Future

package object functions {

  type FutureEitherString[T] = Future[Either[String, T]]
  type EitherString[T] = Either[String, T]
//  implicit val eitherStringMonad = MonadCanFail.monadCanFailForEither[String]
}
