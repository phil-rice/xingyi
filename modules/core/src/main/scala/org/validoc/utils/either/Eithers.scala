package org.validoc.utils.either


object Eithers {

  implicit class EitherPimper[L, R](either: Either[L, R]) {
    def getOrException(exceptionCreator: L => Throwable): R =
      either match {
        case Right(r) => r
        case Left(l) => throw exceptionCreator(l)
      }
  }

}
