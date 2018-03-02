package org.validoc.utils

package object functions {

  type EitherString[T] = Either[String, T]
//  implicit val eitherStringMonad = MonadCanFail.monadCanFailForEither[String]
}
