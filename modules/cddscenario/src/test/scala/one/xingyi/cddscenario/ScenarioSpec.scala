package one.xingyi.cddscenario
import one.xingyi.core.UtilsSpec
import EngineBuilderLanguage._
import one.xingyi.core.reflection.IsDefinedInSourceCodeAt
import one.xingyi.core.strings.ShortPrint
import org.mockito.Mockito._
class ScenarioSpec extends UtilsSpec with ScenarioFixture {

  behavior of "Scenario"

  it should "have an 'IsDefinedInSourceCodeAt" in {
    implicitly[IsDefinedInSourceCodeAt[Scenario[String, String]]] apply snormal shouldBe snormal.definedInSourceCodeAt
  }

  it should "have an 'hasEngineComponentData'" in {
    implicitly[HasEngineComponentData[Scenario[String, String]]] apply snormal shouldBe snormal.data
  }

  it should "have a 'shortPrintFor" in {
    implicitly[ShortPrint[Scenario[String, String]]] apply snormal shouldBe "woman with passport => accept"
  }

  behavior of "Scenario.acceptResult"

  it should "accept anything when no assertions and no result" in {
    val s = Scenario[String, String]("1", None, mock[SingleScenarioLogic[String, String]], List(), mock[EngineComponentData])
    s.acceptResult("1", "any") shouldBe true
    s.acceptResult("2", "") shouldBe true
  }

  it should "accept when no result and assertions return true" in {
    val a1 = mock[ScenarioAssertion[String, String]]
    val a2 = mock[ScenarioAssertion[String, String]]
    when(a1.isTrue("1", "res")) thenReturn (true)
    when(a2.isTrue("1", "res")) thenReturn (true)
    val s = Scenario[String, String]("1", None, mock[SingleScenarioLogic[String, String]], List(a1,a2), mock[EngineComponentData])
    s.acceptResult("1", "res") shouldBe true
  }
  it should "not accept when no result and an assertions returns false" in {
    val a1 = mock[ScenarioAssertion[String, String]]
    val a2 = mock[ScenarioAssertion[String, String]]
    when(a1.isTrue("1", "res")) thenReturn (true)
    when(a2.isTrue("1", "res")) thenReturn (false)
    val s = Scenario[String, String]("1", None, mock[SingleScenarioLogic[String, String]], List(a1,a2), mock[EngineComponentData])
    s.acceptResult("1", "res") shouldBe false
  }

  it should "accept anything only the result when no assertions and result specified" in {
    val s = Scenario[String, String]("1", Some("res"), mock[SingleScenarioLogic[String, String]], List(), mock[EngineComponentData])
    s.acceptResult("1", "any") shouldBe false
    s.acceptResult("1", "res") shouldBe true
  }

  it should "accept anything only the result when result specified and assertions return true" in {
    val a1 = mock[ScenarioAssertion[String, String]]
    val a2 = mock[ScenarioAssertion[String, String]]
    when(a1.isTrue("1", "res")) thenReturn (true)
    when(a2.isTrue("1", "res")) thenReturn (true)
    val s = Scenario[String, String]("1", Some("res"), mock[SingleScenarioLogic[String, String]], List(a1, a2), mock[EngineComponentData])
    s.acceptResult("1", "any") shouldBe false
    s.acceptResult("1", "res") shouldBe true
  }

  it should "not accept anything only the result when result specified and an assertion return false" in {
    val a1 = mock[ScenarioAssertion[String, String]]
    val a2 = mock[ScenarioAssertion[String, String]]
    when(a1.isTrue("1", "res")) thenReturn (true)
    when(a2.isTrue("1", "res")) thenReturn (false)
    val s = Scenario[String, String]("1", Some("res"), mock[SingleScenarioLogic[String, String]], List(a1, a2), mock[EngineComponentData])
    s.acceptResult("1", "any") shouldBe false
    s.acceptResult("1", "res") shouldBe false
  }



}
