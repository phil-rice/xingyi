package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.UtilsSpec

class DataNeededToMakeANewTreeSpec extends UtilsSpec with DecisionTreeFixture {

  behavior of "DecisionTreeFoldingData"

  case class DecisionIssueForTest[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) extends DecisionIssue[P, R]("someIssue")

  val issue1 = DecisionIssueForTest[String, String](conca, snormal)
  val issue2 = DecisionIssueForTest[String, String](concb, sa)

  private val treeAndScenario = TreeAndScenario(treeNormalPassport, snormal2)
  val data = DataNeededToMakeANewTree(treeAndScenario, mock[DTFolderStrategy])

  behavior of "DataNeededToMakeANewTree"

  it should "have the old tree as the tree in the 'TreeAndScenario'" in {
    data.oldTree shouldBe treeNormalPassport
  }
  it should " delegate conclusionAndScenario to the 'TreeAndScenario'" in {
    data.conclusionAndScenario shouldBe treeAndScenario.conclusionAndScenario
  }
  it should "delegate lens to the 'TreeAndScenario'" in {
    data.lens shouldBe treeAndScenario.lens
  }
}
