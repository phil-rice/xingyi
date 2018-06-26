package one.xingyi.cddengine
import one.xingyi.core.UtilsSpec

class DecisionTreeSpec extends UtilsSpec with DecisionTreeFixture {

  behavior  of "DecisionTree"

  it should "allow parents to be found from situation" in {
    DecisionTree.findWithParents(treeNormalGun, "gun") shouldBe List(dNormalGun,concGun )
    DecisionTree.findWithParents(treeNormalGun, "password") shouldBe List(dNormalGun,concNormal )
    DecisionTree.findWithParents(treeNormalPassport, "gun") shouldBe List(dNormalPassport,concNoPassport)
  }
  it should "allow parents to be found from scenario" in {
    DecisionTree.findWithParentsForScenario(treeNormalGun)(sgun) shouldBe List(dNormalGun,concGun )
    DecisionTree.findWithParentsForScenario(treeNormalGun)(snormal) shouldBe List(dNormalGun,concNormal )
    DecisionTree.findWithParentsForScenario(treeNormalPassport)(sgunNoPassport) shouldBe List(dNormalPassport,concNoPassport)
  }
}
