package org.validoc.utils.either


trait EitherOps {

  implicit class EitherPimper[L, R](either: Either[L, R]) {
    def getOrException(exceptionCreator: L => Throwable): R =
      either match {
        case Right(r) => r
        case Left(l) => throw exceptionCreator(l)
      }
  }

}

object EitherOps extends EitherOps