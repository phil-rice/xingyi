/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sampleServer
import one.xingyi.core.UtilsSpec
import one.xingyi.core.UtilsSpec
import one.xingyi.core.monad.AsyncForScalaFuture
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.sample.{BillboardSetup, FnordSetup, domain}
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.sample.domain.{Production, ProductionId, Promotion, PromotionQuery}
import one.xingyi.json4s.Json4sParser._
class FnordSetupSpec extends UtilsSpec with AllProducersSetup {

  behavior of "FnordSetup"
  lazy val setup = new FnordSetup()

  it should "be configured" in {
    setup.async.isInstanceOf[AsyncForScalaFuture] shouldBe true
    await(setup.production(domain.ProductionId("1",false))) shouldBe Production("from 1")
  }
}
