/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.monad
import one.xingyi.core.FunctionFixture
import one.xingyi.core.language.{AnyLanguage, MonadLanguage}

import scala.language.higherKinds

trait AbstractMonadHasStateTests[M[_]] extends ContainerSpec[M] with AnyLanguage with MonadLanguage with FunctionFixture {

  implicit def monadWithState: MonadWithState[M]

  behavior of s"MonadWithState for ${monadWithState.getClass.getSimpleName}"

  val lv1 = LocalVariable[Int]()
  val lv2 = LocalVariable[Int]()

  it should "have a mapWith that works when LocalVariable has not been set" in {
    1.liftM[M].mapWith(lv1)(fn2(1, Seq[Int](), 2)) |> getT shouldBe 2
  }

  it should "have a mapWith that works when the LocalVariable has been set" in {
    val x = 1.liftResultAndPut(lv1, 10).map { x => x }.mapWith(lv1)(fn2(1, Seq(10), 2)) |> getT shouldBe 2
    //    1.liftResultAndPut(lv1, 10).mapWith(lv2)(fn2(1, Seq[Int](), 2)) |> getT shouldBe 2
  }

  it should "have a putIntoMethod" in {
    monadWithState.clear
    1.liftM.putInto(lv1, 10).mapWith(lv1)(fn2(1, Seq(10), 2)) |> getT shouldBe 2
    monadWithState.clear
    1.liftM.putInto(lv1, 10).putInto(lv1, 20).mapWith(lv1)(fn2(1, Seq(10, 20), 2)) |> getT shouldBe 2
    monadWithState.clear
    1.liftResultAndPut(lv1, 10).putInto(lv1, 20).mapWith(lv2)(fn2(1, Seq[Int](), 2)) |> getT shouldBe 2
    monadWithState.clear
    1.liftResultAndPut(lv1, 10).putInto(lv2, 20).mapWith(lv1)(fn2(1, Seq(10), 2)) |> getT shouldBe 2
  }

  it should "have a mapState method " in {
    monadWithState.clear
    1.liftResultAndPut(lv1, 10).mapState(lv1)(fn(Seq(10), 2)) |> getT shouldBe 2
    monadWithState.clear
    1.liftResultAndPut(lv1, 10).mapState(lv1)(fn(Seq(10), 2)).mapWith(lv1)(fn2(2, Seq(10), 3)) |> getT shouldBe 3
  }

  it should "keep the state even with map methods " in {
    monadWithState.clear
    1.liftResultAndPut(lv1, 10).map(_ * 2).mapWith(lv1)(fn2(2, Seq(10), 4)) |> getT shouldBe 4
  }
  it should "keep the state even with flatMap methods " in {
    monadWithState.clear
    1.liftResultAndPut(lv1, 10).flatMap(fn(1, 2.liftM)).mapWith(lv1)(fn2(2, Seq(10), 4)) |> getT shouldBe 4
  }

  it should "clear the state " in {
    //So this is an awkward test which tells us that perhaps this is the wrong model for clear (which is plain clunky)
    //The idea of this is that if there are any thread locals they are cleaned up... but basically this is pretty messy...
    1.liftResultAndPut(lv1, 10) |> getT
    2.liftResultAndPut(lv1, 20) |> getT

    monadWithState.clear.mapState(lv1)(sq => sq) |> getT shouldBe Seq()
  }
}



