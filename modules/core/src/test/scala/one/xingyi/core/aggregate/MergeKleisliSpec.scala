/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.aggregate

import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, Monad, MonadCanFailWithException, ScalaFutureAsAsyncAndMonadAndFailer}
import one.xingyi.core.{AsyncFixture, UtilsSpec}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success}

abstract class MergeKleisliSpec[M[_], Fail](implicit monadCanFail: MonadCanFailWithException[M, Fail], async: Async[M]) extends UtilsSpec with AsyncFixture[M] {

  type Kleisli[Req, Res] = Req => M[Res]
  val merger = new MergeForTaglessLanguage[Kleisli] with MergeKleisli[M] {
    override protected implicit def monad: Monad[M] = monadCanFail
  }

  import merger._

  behavior of "MergeKleisli"

  val runtimeException = new RuntimeException

  val k1: Int => M[String] = kleisli(1, Success("one"))
  val k2: Double => M[String] = kleisli(1.0, Success("two"))
  val k3: Int => M[String] = kleisli(1, Success("three"))
  val k4: Double => M[String] = kleisli(1, Success("four"))
  val kf: Int => M[String] = kleisli(1, Failure(runtimeException))

  implicit object FindReqForStringDouble extends FindReq[String, Double] {
    override def apply(v1: String): Double = Integer.parseInt(v1).toDouble
  }

  implicit object FindReqForStringInt extends FindReq[String, Int] {
    override def apply(v1: String): Int = Integer.parseInt(v1)
  }

  it should "merge 2 kleislis" in {
    val composite: String => M[String] = merge[Int, String](k1).and(k2).into[String, String]((req, i1, i2) => s"$req/$i1/$i2")
    composite("1").await() shouldBe "1/one/two"
  }
  it should "merge 3 kleislis" in {
    val composite: String => M[String] = merge[Int, String](k1) and k2 and k3 into[String, String] ((req, i1, i2, i3) => s"$req/$i1/$i2/$i3")
    composite("1").await() shouldBe "1/one/two/three"
  }
  it should "merge 4 kleislis" in {
    val composite: String => M[String] = merge[Int, String](k1) and k2 and k3 and k4 into[String, String] ((req, i1, i2, i3, i4) => s"$req/$i1/$i2/$i3/$i4")
    composite("1").await() shouldBe "1/one/two/three/four"
  }
}

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class ScalaFutureMergeKleisliSpec extends MergeKleisliSpec[Future, Throwable] with ScalaFutureAsAsyncAndMonadAndFailer
