package one.xingyi.cddengine
import one.xingyi.core.UtilsSpec
import one.xingyi.core.reflection.IsDefinedInSourceCodeAt

class DecisionTreeNodeSpec extends UtilsSpec with DecisionTreeFixture{

  behavior of "DecisionNode"

  it should "have an 'isDefinedAt'" in {
    (implicitly[IsDefinedInSourceCodeAt[DecisionNode[String,String]]] apply dNormalPassport ).toString shouldBe "(ScenarioFixture.scala:13)"
  }
  behavior of "ConclusionNode"

  it should "have an 'isDefinedAt'" in {
    (implicitly[IsDefinedInSourceCodeAt[ConclusionNode[String,String]]] apply concNormal ).toString shouldBe "(ScenarioFixture.scala:11)"
  }

}
