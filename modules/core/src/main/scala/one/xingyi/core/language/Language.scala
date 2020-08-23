/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language

import one.xingyi.core.functions._
import one.xingyi.core.json.{JsonParserLanguage, JsonWriterLanguage}
import one.xingyi.core.monad.{Async, MonadCanFail, MonadWithException}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}


object AsyncLanguage extends AsyncLanguage
trait AsyncLanguage {

  def liftTry[M[_], T](tryT: Try[T])(implicit monad: MonadWithException[M]): M[T] = tryT match {
    case Success(t) => monad.liftM(t)
    case Failure(t) => monad.exception(t)
  }

  implicit class AsyncFunctionPimper[M[_], From, To](fn: From => M[To])(implicit async: Async[M]) {
    def turnSynchronous: From => To =
      from => async.await(fn(from))
  }

  implicit class AsyncPimper[M[_], T](m: M[T])(implicit async: Async[M]) {
    def await(): T = async.await(m)
  }
  implicit class FailurePimper[Failure](f: Failure) {
    def fail[M[_], T](implicit async: MonadCanFail[M, Failure]): M[T] = async.fail[T](f)
  }

}

object Language extends AnyLanguage with FunctionLanguage with MonadFunctionLanguage with AsyncLanguage with JsonParserLanguage with JsonWriterLanguage with SuccessOrFailLanguage
