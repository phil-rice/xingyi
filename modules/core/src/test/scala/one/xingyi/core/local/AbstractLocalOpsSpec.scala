/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.local

import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, Monad}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class AbstractLocalOpsSpec[M[_] : Monad : Async](name: String)(implicit localOps: LocalOps[M]) extends FlatSpec with Matchers with LocalOpsPimper[M] {

  behavior of s"LocalOps for $name"

  def goThroughMapAndFlatMap[T: ClassTag]() = 1.liftM[M].flatMap { x: Int => x.liftM[M] }.map { x: Int => getFromLocalStore[Int] }.await


  it should "allow a local to be created and values put and get in it" in {
    clearlocalStore[Int]()
    getFromLocalStore[Int]() shouldBe None
    putInlocalStore(1)

    getFromLocalStore[Int]() shouldBe Some(1)
    putInlocalStore(2)

    getFromLocalStore[Int]() shouldBe Some(2)
    clearlocalStore[Int]()
    getFromLocalStore[Int]() shouldBe None

  }


  it should "allow a local to be created and the values available after maps and flatmaps" in {
    clearlocalStore()

    goThroughMapAndFlatMap() shouldBe None
    putInlocalStore(1)
    goThroughMapAndFlatMap() shouldBe Some(1)
    putInlocalStore(2)
    goThroughMapAndFlatMap() shouldBe Some(2)
    clearlocalStore[Int]()
    goThroughMapAndFlatMap() shouldBe None
  }
}

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class ScalaFutureLocallOpsSpec extends AbstractLocalOpsSpec[Future]("scala future")
