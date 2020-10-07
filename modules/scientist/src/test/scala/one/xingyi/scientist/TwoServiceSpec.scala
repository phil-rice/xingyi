/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scientist

import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.monad.IdentityMonad._
import one.xingyi.core.{FunctionFixture, UtilsSpec}

import scala.language.higherKinds
import scala.util.Success


class TwoServiceSpec extends UtilsSpec with  FunctionFixture {

  behavior of "TwoService"

  it should "Call just process the left if only the left is selected" in {
    val twoServiceProcessor = new TwoServiceProcessor[IdentityMonad, Throwable, String, String] {
      override def selectAndTransformRequests: String => IdentityMonad[(Option[String], Option[String])] = k1("from", (Some("from1"), None))
      override def postProcessResults: TwoServiceMerger[IdentityMonad, String, String] = (raw, opt1, opt2) => {
        raw shouldBe "from"
        opt1 shouldBe Some("from1", "to1".liftM[IdentityMonad])
        opt2 shouldBe None
        "final".liftM[IdentityMonad]
      }
    }
    val service = new TwoService[IdentityMonad, Throwable, String, String](twoServiceProcessor, k1("from1", "to1"), k1("from2", "to2"))
    service("from").value shouldBe Success("final")
  }
  it should "Call just process the right if only the left is selected" in {
    val twoServiceProcessor = new TwoServiceProcessor[IdentityMonad, Throwable, String, String] {
      override def selectAndTransformRequests: String => IdentityMonad[(Option[String], Option[String])] = k1("from", (None, Some("from2")))
      override def postProcessResults: TwoServiceMerger[IdentityMonad, String, String] = (raw, opt1, opt2) => {
        raw shouldBe "from"
        opt1 shouldBe None
        opt2 shouldBe Some("from2", "to2".liftM[IdentityMonad])
        "final".liftM[IdentityMonad]
      }
    }
    val service = new TwoService[IdentityMonad, Throwable, String, String](twoServiceProcessor, k1("from1", "to1"), k1("from2", "to2"))
    service("from").value shouldBe Success("final")
  }
  it should "Call just process the left and right if both selected" in {
    val twoServiceProcessor = new TwoServiceProcessor[IdentityMonad, Throwable, String, String] {
      override def selectAndTransformRequests: String => IdentityMonad[(Option[String], Option[String])] = k1("from", (Some("from1"), Some("from2")))
      override def postProcessResults: TwoServiceMerger[IdentityMonad, String, String] = (raw, opt1, opt2) => {
        raw shouldBe "from"
        opt1 shouldBe Some("from1", "to1".liftM[IdentityMonad])
        opt2 shouldBe Some("from2", "to2".liftM[IdentityMonad])
        "final".liftM[IdentityMonad]
      }
    }
    val service = new TwoService[IdentityMonad, Throwable, String, String](twoServiceProcessor, k1("from1", "to1"), k1("from2", "to2"))
    service("from").value shouldBe Success("final")
  }

}
