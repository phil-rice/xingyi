package org.validoc.utils.success

import org.validoc.utils.functions.MonadCanFailWithException
import org.validoc.utils.language.Language._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

object SuccessState {

  def succeeded = "success"
  def exception = "exception"
  def failed = "failed"

  def foldWithString[M[_], Fail, Req, Res](m: M[Req], fnThrowable: (String, Throwable) => Res,
                                           fnFail: (String, Fail) => Res,
                                           fnSuccess: (String, Req) => Res)(implicit monad: MonadCanFailWithException[M, Fail]): M[Res] = m.foldWithExceptionAndFail[Fail, Res](
    throwable => fnThrowable(failed, throwable).liftM,
    fail => fnFail(failed, fail).liftM,
    res => fnSuccess(succeeded, res).liftM
  )
  def sideffectWithString[Fail, T](fnThrowable: (String, Throwable) => Unit,
                                   fnFail: (String, Fail) => Unit,
                                   fnSuccess: (String, T) => Unit)(tryR: Try[Either[Fail, T]]): Unit = {
    tryR match {
      case Failure(t) => fnThrowable(exception, t)
      case Success(Left(fail)) => fnFail(failed, fail)
      case Success(Right(t)) => fnSuccess(succeeded, t)
    }
  }
}

