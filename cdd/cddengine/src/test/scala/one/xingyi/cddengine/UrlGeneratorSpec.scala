/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import java.text.MessageFormat

import one.xingyi.cddscenario.Scenario
import one.xingyi.core.UtilsSpec
import one.xingyi.core.misc.IdMaker

import scala.collection.concurrent.TrieMap

class UrlGeneratorSpec extends UtilsSpec with EngineFixture {
  class SimpleUrlGenerator[P, R](pattern: String = "{0}_{1}.html") extends EngineUrlGenerators[P, R] with IdMaker {
    val map = new TrieMap[Any, Int]()
    def getOrUpdate(a: Any): String = map.getOrElseUpdate(a, getNextId).toString
    def url[T](typeName: String)(t: T) = MessageFormat.format(pattern, typeName, getOrUpdate(t))
    def engineurl[T](typeName: String)(t: T) = MessageFormat.format(pattern, typeName, "index")
    override def engine: UrlGenerator[Engine[P, R]] = engineurl("engine")
    override def usecase: UrlGenerator[UseCase[P, R]] = url("usecase")
    override def scenario: UrlGenerator[Scenario[P, R]] = url("scenario")
  }


  behavior of "SimpleUrlGenerator"

  val s = new SimpleUrlGenerator[String, String]()

  it should "return the same url for the same object" in {
    s.engine(e) shouldBe "engine_index.html"
    s.engine(e) shouldBe "engine_index.html"
    s.usecase(usecase1) shouldBe "usecase_0.html"
    s.usecase(usecase2) shouldBe "usecase_1.html"
    s.usecase(usecase1) shouldBe "usecase_0.html"
    s.scenario(snormal) shouldBe "scenario_2.html"
    s.scenario(snormal) shouldBe "scenario_2.html"
    s.scenario(snormal2) shouldBe "scenario_3.html"
  }
}
