/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.success

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.UtilsSpec

import scala.util.{Failure, Success, Try}

class SuccessStateSpec extends UtilsSpec {

  behavior of "SucessState"
  def setup(fn: (Try[Either[String, String]] => Unit, AtomicReference[String]) => Unit) = {
    val ref = new AtomicReference[String]()
    val state = SuccessState.sideffectWithString[String, String]((str, t) => ref.set("T" + str + t), (str, t) => ref.set("F" + str + t), (str, t) => ref.set("G" + str + t)) _
    fn(state, ref)
  }

  it should "Fold with successes" in {
    setup { (state, ref) =>
      state(Success(Right("xxx")))
      ref.get shouldBe "Gsuccessxxx"
    }
  }
  it should "Fold with failures" in {
    setup { (state, ref) =>
      state(Success(Left("fff")))
      ref.get shouldBe "Ffailedfff"
    }
  }
  it should "Fold with exceptions" in {
    setup { (state, ref) =>
      state(Failure(new RuntimeException("someMsg")))
      ref.get shouldBe "Texceptionjava.lang.RuntimeException: someMsg"
    }
  }
}
