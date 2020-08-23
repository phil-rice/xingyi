/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.streams

import one.xingyi.core.CoreSpec
import one.xingyi.core.language.Language.AnyOps
import one.xingyi.core.monad.{Async, IdentityMonad, MonadWithException, SuccessOrFail}
import one.xingyi.core.streams.StreamLanguage.StreamPimper

import scala.language.higherKinds
import scala.util.Try

abstract class StreamLanguageTest[M[_] : MonadWithException : Async, S[_]](implicit s: SuccessOrFail[S]) extends CoreSpec {

  behavior of "StreamLanguage"
  val e = new RuntimeException("Should not be visible")
  def mapFn(i: Int) = if (i % 3 == 0) throw e else i.toString.liftM[M]

  it should "batchMapOverKleislis a stream" in {
    Stream.from(1).batchMapOverKleislis[M, S, String](2, mapFn).take(6).toList shouldBe
      List("1".liftM[S], "2".liftM[S], s.exception(e), "4".liftM[S], "5".liftM[S], s.exception(e))
  }

  it should "map/reduce using the native streams map and foldLeft" in {
    Stream.from(1).batchMapOverKleislis[M, S, String](2,mapFn).take(10).addAll[String](v => s.fold(v, e => "e", (x: String) => x)) shouldBe "12e45e78e10"
  }

}
class IdentityMonadStreamLanguageTest extends StreamLanguageTest[IdentityMonad, Try]
