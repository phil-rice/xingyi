/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language
import one.xingyi.core.{FunctionFixture, UtilsSpec}
import one.xingyi.core.monad.{Async, IdentityMonad, Monad}
import scala.language.higherKinds

abstract class AndAfterKleisliSpec[M[_] ](implicit val monad: Monad[M], async: Async[M]) extends UtilsSpec with AndAfterKleisli[M] with AnyLanguage with AsyncLanguage with  FunctionFixture {


  behavior of "AndAfterKleisli"

  it should "have an 'andAfter' that returns a new function " in {
    val f: Int => M[Int] = andAfter(fn(1, 2.liftM), fn(2, 3))
    f(1).await() shouldBe 3
  }
  it should "have an 'andAfterK' that returns a new function " in {
    val f: Int => M[Int] = andAfterK(fn(1, 2.liftM), fn(2, 3.liftM))
    f(1).await() shouldBe 3
  }
}

class IdentityAndAfterKleisliSpec extends AndAfterKleisliSpec[IdentityMonad]
