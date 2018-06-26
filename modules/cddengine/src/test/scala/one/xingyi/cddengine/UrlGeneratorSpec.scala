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
