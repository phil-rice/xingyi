/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core

import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{MonadCanFail, ScalaFutureAsAsyncAndMonadAndFailer}
import org.mockito.Mockito._

import scala.concurrent.Future
import scala.util.{Failure, Success}

class LanguageSpec extends UtilsSpec with ScalaFutureAsAsyncAndMonadAndFailer with AsyncFixture[Future] with FunctionFixture {

  val runtimeException = new RuntimeException
  behavior of "tryPimper"

  it should "allow a lift try that lifts a try into a liftable" in {
    Success(1).liftTry[Future].await() shouldBe 1
    intercept[RuntimeException](Failure(runtimeException).liftTry[Future].await()) shouldBe runtimeException
  }

  behavior of "AsyncLanguage"

  it should "have a liftTry method" in {
    liftTry(Success(1)).await() shouldBe 1
    intercept[RuntimeException](liftTry(Failure(runtimeException)).await) shouldBe runtimeException
  }

  behavior of "failure"

  it should "lift the failure into a MonadWithFailure" in {
    implicit val monad = mock[MonadCanFail[Option, String]]
    when(monad.fail[Int]("123")) thenReturn Some(42)
    "123".fail[Option, Int] shouldBe Some(42)
  }

  behavior of "MonadFunctionPimper"

  it should "have a kleisli composer that passes the output to a function|=>" in {
    (kleisli(1, Success(2)) |=> fn(2, 3)) (1).await() shouldBe 3
    intercept[RuntimeException]((kleisli(1, Failure(runtimeException)) |=> fn(2, 3)) (1).await()) shouldBe runtimeException
    intercept[RuntimeException]((kleisli(1, Success(2)) |=> fn(2, throw runtimeException)) (1).await()) shouldBe runtimeException
  }

  it should "have a kleisli composer that passes the request as well as the result to a function Req => Res => Res2 using |=+>[" in {

    val k1: (Int => Future[Int]) = kleisli(1, Success(2))
    val k2: Int => Int => Int = fn2Curried(1, 2, 3)
    val composite: Int => Future[Int] = k1 |=+> k2
    composite(1).await() shouldBe 3
  }
  it should "have a kleisli composer that passes the request as well as the result to a function Req => Res => Res2 using |==+>[" in {
    val k1: (Int => Future[Int]) = kleisli(1, Success(2))
    val k2: Int => Int => Future[Int] = fn2Curried(1, 2, Future.successful(3))
    val composite: Int => Future[Int] = k1 |==+> k2
    composite(1).await() shouldBe 3
  }
  it should "have a kleisli arrow using |==>" in {
    val composite: (Int => Future[Int]) = (kleisli(1, Success(2)) |==> kleisli(2, Success(3)))
    composite(1).await() shouldBe 3
    intercept[RuntimeException]((kleisli(1, Failure(runtimeException)) |==> kleisli(2, Success(3))) (1).await) shouldBe runtimeException
    intercept[RuntimeException]((kleisli(1, Success(2)) |==> kleisli(2, Failure(runtimeException))) (1).await) shouldBe runtimeException
  }

}
