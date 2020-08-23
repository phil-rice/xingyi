/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.success
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.MonadCanFailWithException

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

object SuccessState {

  def succeeded = "success"
  def exception = "exception"
  def failed = "failed"

//  def foldWithString[M[_], Fail, Req, Res](m: M[Req], fnThrowable: (String, Throwable) => Res,
//                                           fnFail: (String, Fail) => Res,
//                                           fnSuccess: (String, Req) => Res)(implicit monad: MonadCanFailWithException[M, Fail]): M[Res] = m.foldWithExceptionAndFail[Fail, Res](
//    throwable => fnThrowable(failed, throwable).liftM,
//    fail => fnFail(failed, fail).liftM,
//    res => fnSuccess(succeeded, res).liftM
//  )
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

