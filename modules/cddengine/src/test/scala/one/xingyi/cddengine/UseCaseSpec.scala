package one.xingyi.cddengine

import one.xingyi.cddscenario._
import one.xingyi.core.UtilsSpec
import org.scalatest.mockito.MockitoSugar
import scala.language.postfixOps

trait UseCaseFixture extends EngineBuilderLanguage1 with MockitoSugar {

  implicit val agg: ScenarioAggregator2[String, String] = NullScenarioAggregator2.nullAggregator

  val cd1 = mock[EngineComponentData]

  val s1: Scenario[String, String] = scenario("some1") produces ("result1") scenario
  val s2 = scenario("some2") produces ("result2") scenario
  val s3 = scenario("some3") produces ("result3") scenario
  val s4 = scenario("some4") produces ("result4") scenario
}

abstract class AbstractUseCaseSpec[UC <: UseCase[String, String]](name: String) extends UtilsSpec with UseCaseFixture {
  behavior of name

  def UCwith123: UC

  def UCwith12: UC

  def UCwith1: UC

  it should "have a copy with only method that returns a uc with only the scenarios" in {
    UCwith123.copyWithOnly(List(s1, s2)).allScenarios shouldBe List(s1, s2)
    UCwith123.copyWithOnly(List(s2, s1)).allScenarios shouldBe List(s1, s2)
    UCwith12.copyWithOnly(List(s2, s1)).allScenarios shouldBe List(s1, s2)
    UCwith1.copyWithOnly(List(s2, s1)).allScenarios shouldBe List(s1)
    UCwith1.copyWithOnly(List()).allScenarios shouldBe List()
  }

  it should "have an allScenario" in {
    UCwith123.allScenarios shouldBe List(s1, s2, s3)
    UCwith12.allScenarios shouldBe List(s1, s2)
    UCwith1.allScenarios shouldBe List(s1)
  }

  it should "have a data method" in {
    UCwith1.data shouldBe cd1
  }
}

class SimpleUseCaseSpec extends AbstractUseCaseSpec[SimpleUseCase[String, String]]("SimpleUseCase") {


  override def UCwith123: SimpleUseCase[String, String] = SimpleUseCase(cd1, List(s1, s2, s3))

  override def UCwith12: SimpleUseCase[String, String] = SimpleUseCase(cd1, List(s1, s2))

  override def UCwith1: SimpleUseCase[String, String] = SimpleUseCase(cd1, List(s1))

  it should "have an allusecases method that returns empty list" in {
    UCwith1.allUseCases shouldBe List()
    UCwith12.allUseCases shouldBe List()
    UCwith123.allUseCases shouldBe List()
  }
}

class CompositeUseCaseSpec extends AbstractUseCaseSpec[CompositeUseCase[String, String]]("CompositeUseCase") {
  val cd2 = mock[EngineComponentData]

  def uc4: SimpleUseCase[String, String] = SimpleUseCase(cd2, List(s3))

  def uc3: SimpleUseCase[String, String] = SimpleUseCase(cd2, List(s3))

  def uc2: SimpleUseCase[String, String] = SimpleUseCase(cd2, List(s2))

  def uc1: SimpleUseCase[String, String] = SimpleUseCase(cd2, List(s1))

  override def UCwith123: CompositeUseCase[String, String] = CompositeUseCase(List(uc1, uc2, uc3), cd1)

  override def UCwith12: CompositeUseCase[String, String] = CompositeUseCase(List(uc1, uc2), cd1)

  override def UCwith1: CompositeUseCase[String, String] = CompositeUseCase(List(uc1), cd1)

  it should "have an allusecases method that returns the composite use cases" in {
    UCwith1.allUseCases shouldBe List(uc1)
    UCwith12.allUseCases shouldBe List(uc1, uc2)
    UCwith123.allUseCases shouldBe List(uc1, uc2,uc3)
    CompositeUseCase(List(UCwith123, uc4), cd1).allUseCases shouldBe List(UCwith123, uc4)
  }
}
