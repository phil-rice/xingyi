/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language

import one.xingyi.core.endpoint.EndPoint
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer
import one.xingyi.core.{AsyncFixture, UtilsSpec}

import scala.concurrent.Future
import scala.util.Success

class MicroserviceComposerSpec extends UtilsSpec with AsyncFixture[Future] with ScalaFutureAsAsyncAndMonadAndFailer with MicroserviceComposers[Future] {

  behavior of "MicroserviceComposer"

  it should "use |+| compose a kleisli with a transformer" in {
    val k1 = kleisli[Int, String](1, Success("two"))
    val composite = k1 |+| kleisliTransformer[Int, String, String, Int](k1, kleisli("five", Success(6)))
    await(composite("five")) shouldBe 6
  }
  it should "use |++| to turn a kleisli into an endpoint" in {
    val k1 = kleisli[Int, String](1, Success("two"))
    val endpoint = mock[EndPoint[Future, Int, String]]
    val actual = k1 |++| { k: (Int => Future[String]) => endpoint }
    actual shouldBe endpoint
  }

}
