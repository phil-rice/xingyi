/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.finatra

import com.twitter.util.{Future => TFuture}
import one.xingyi.core.CoreSpec
import one.xingyi.core.functions._
import one.xingyi.core.http.Failer.failerForThrowable
import one.xingyi.core.http.ResponseCategoriserSpec
import one.xingyi.core.local.AbstractLocalOpsSpec
import one.xingyi.core.monad.{AbstractAsyncTests, AbstractMonadCanFailWithFailWithExceptionAsThrowableTests, AbstractMonadHasStateTests, MonadWithState}
import one.xingyi.core.objectify.AbstractObjectifySpec
import one.xingyi.finatra.FinatraImplicits.ImplicitsForTest._
import one.xingyi.finatra.FinatraImplicits._

class TwitterFutureSpec extends AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[TFuture] with CoreSpec with AbstractAsyncTests[TFuture] {
  override def async = asyncForTwitter
  override def monad = asyncForTwitter

}
class TwitterFutureAsyncPimperTests extends AbstractAsyncPimperTests[TFuture]
class TwitterFuturHasStateTests extends AbstractMonadHasStateTests[TFuture] {
  override implicit def monadWithState: MonadWithState[TFuture] = asyncForTwitter
  override def liftA[T](t: T): TFuture[T] = asyncForTwitter.liftM(t)
  override def getT[X](a: TFuture[X]): X = asyncForTwitter.await(a)
}

class FutureObjectifySpec extends AbstractObjectifySpec[TFuture, Throwable]

class TwitterFutureResponseCategoriserSpec extends ResponseCategoriserSpec[TFuture, Throwable]

class TwitterFutureLocalOpsSpec extends AbstractLocalOpsSpec[TFuture]("scala future")
